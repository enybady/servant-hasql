{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
  ( server
  ) where

import Control.Exception (throw)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Data.Swagger
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
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Swagger
import Data.Aeson.Encode.Pretty   (encodePretty)


data Routes route = Routes
    { getAllNodes :: route :- "graph" :> "node" :> Get '[JSON] [Node]
    }
  deriving (Generic)

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type API = SwaggerAPI :<|> ToServantApi Routes

todoSwagger :: Swagger
todoSwagger = toSwagger apiPsql

apiPsql :: Proxy (ToServantApi Routes)
apiPsql = genericApi (Proxy :: Proxy Routes)

api :: Proxy API
api = Proxy 

serverr :: Pool -> Server API
serverr pool = return todoSwagger :<|> (hoistServer apiPsql appMToHandler $ getNodes)
  where
    appMToHandler m = runReaderT m pool

app :: Pool -> Application
app pool = serve api $ serverr pool

server :: IO ()
server = do
  pool <- acquire settings
  run 8080 $ app pool
  where
    settings = (1, 1, Connection.settings "localhost" (fromInteger 5432) "poster" "password" "")

class MonadIO m =>
      MonadDB m
  where
  runSession :: Session a -> m (Either UsageError a)

data Node =
  Node
    { nodeId :: Int
    , label  :: String
    } deriving (Eq, Show, Generic)

instance FromJSON Node

instance ToJSON Node

instance ToSchema Node

getNodes :: (MonadIO m, MonadError ServerError m, MonadReader Pool m) => m [Node]
getNodes = do
  pool <- ask
  result <- liftIO $ use pool selectNodesSession
  case result of
    Right nodes -> pure nodes
    Left error -> parseUsageError error
  where
    parseUsageError (ConnectionError (Just msg)) = throw500 msg
    parseUsageError (ConnectionError (Nothing)) = throw500 "Database connection error"
    parseUsageError (SessionError (Session.QueryError _ _ msg)) = throw500 $ BS.pack $ show msg
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}

selectNodesSession :: Session [Node]
selectNodesSession = Session.statement () selectNodesStatement

selectNodesStatement :: Statement () [Node]
selectNodesStatement =
  Hasql.Statement.Statement
    "SELECT id, label FROM nodes"
    mempty
    (Decoders.rowList $ Node
      <$> (Decoders.column $ Decoders.nonNullable $ fromEnum <$> Decoders.int8)
      <*> (Decoders.column $ Decoders.nonNullable $ T.unpack <$> Decoders.text))
    True
    
writeSwaggerJSON :: IO ()
writeSwaggerJSON = LBS.writeFile "example/swagger.json" (encodePretty todoSwagger)
