module Lib
  ( server
  ) where

import Control.Exception (throw)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Profunctor
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Decoders (Row, Value)
import qualified Hasql.Decoders as Decoders
import Hasql.Pool
import Hasql.Session (QueryError, Session)
import qualified Hasql.Session as Session
import Hasql.Statement
import qualified Hasql.TH as TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = "users" :> Get '[ JSON] [User]

api :: Proxy API
api = Proxy

app :: Pool -> Application
app pool = serve api $ hoistServer api appMToHandler $ getUsers
  where
    appMToHandler :: AppM a -> Handler a
    appMToHandler m = runReaderT (runAppM m) pool

server :: IO ()
server = do
  pool <- acquire settings
  run 8080 $ app pool
  where
    settings = (1, 1, "host=localhost port=5432 user=lupusanay dbname=postgres password=qwerty")

class MonadIO m =>
      MonadDB m
  where
  runSession :: Session a -> m (Either UsageError a)

newtype AppM a =
  AppM
    { runAppM :: ReaderT (Pool) (Handler) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AppM where
  runSession sess =
    AppM $ do
      pool <- ask
      result <- liftIO $ use pool sess
      runAppM $ pure result

data User =
  User
    { username :: String
    , password :: String
    }
  deriving (Show, Generic)

instance FromJSON User

instance ToJSON User

getUsers :: (MonadDB m, MonadError ServerError m) => m [User]
getUsers = do
  result <- runSession allUsersSession
  case result of
    Right users -> pure users
    Left error -> parseUsageError error
  where
    parseUsageError (ConnectionError (Just msg)) = throw500 msg
    parseUsageError (ConnectionError (Nothing)) = throw500 "Database connection error"
    parseUsageError (SessionError (Session.QueryError _ _ msg)) = throw500 $ BS.pack $ show msg
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}

allUsersSession :: Session [User]
allUsersSession = Session.statement () allUsers

allUsers :: Statement () [User]
allUsers = rmap tuplesToUsers [TH.vectorStatement| select name :: text, password :: text from "users"|]
  where
    tupleToUser (name, pass) = User (T.unpack name) (T.unpack pass)
    tuplesToUsers vec = V.toList $ V.map tupleToUser vec