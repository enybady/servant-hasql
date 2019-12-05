module Lib
  ( server
  ) where

import Control.Exception (throw)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import Hasql.Decoders (Row, Value)
import qualified Hasql.Decoders as Decoders
import Hasql.Pool
import Hasql.Session (Session)
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

data User =
  User
    { username :: String
    , password :: String
    }
  deriving (Show, Generic)

instance FromJSON User

instance ToJSON User

class (Monad m, MonadIO m) =>
      MonadDB m
  where
  withPool :: (Pool -> m a) -> m a

newtype AppM a =
  AppM
    { runAppM :: ReaderT (Pool) (Handler) a
    }
  deriving (Functor, Applicative, Monad, MonadIO, Generic, MonadError ServerError)

instance MonadDB AppM where
  withPool f =
    AppM $ do
      pool <- ask
      runAppM (f pool)

instance MonadFail AppM where
  fail msg = throwError $ err500 {errBody = BSL.fromStrict $ BS.pack $ show msg}

getUsers :: (MonadDB m, MonadFail m) => m [User]
getUsers = do
  Right users <- withPool $ \pool -> liftIO $ use pool allUsersSession
  pure $ users

allUsersSession :: Session [User]
allUsersSession = do
  userTuples <- Session.statement () allUsers
  let users = V.toList $ V.map decodeUserFromTuple userTuples
  pure $ users

decodeUserFromTuple :: (T.Text, T.Text) -> User
decodeUserFromTuple (name, password) = User (T.unpack name) (T.unpack password)

allUsers :: Statement () (V.Vector (T.Text, T.Text))
allUsers = [TH.vectorStatement| select name :: text, password :: text from "users"|]

userRow :: Row User
userRow =
  User <$> (T.unpack <$> (Decoders.column . Decoders.nonNullable) Decoders.text) <*>
  (T.unpack <$> (Decoders.column . Decoders.nonNullable) Decoders.text)