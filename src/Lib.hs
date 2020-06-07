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
import qualified Hasql.Encoders as Encoders
import Hasql.Pool
import Hasql.Session (QueryError, Session)
import qualified Hasql.Session as Session
import Hasql.Statement
import qualified Hasql.TH as TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Swagger
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Int (Int16)
import Data.ByteString.Char8 (append)
import NodeData
import HasqlHelper
import Data.Either (fromRight)


data Routes route = Routes
    { getAllNodes :: route :- "graph" :> "node" :> Get '[JSON] [Node]
    , getNeighboursNodes :: route :- "graph" :> "node" :> Capture "id" Integer :> "neighbours" :> Get '[JSON] [Node]
    , putNode :: route :- "graph" :> "node" :> ReqBody '[JSON] String :> Put '[JSON] Int
    , deleteNode :: route :- "graph" :> "node" :> Capture "id" Integer :> DeleteNoContent '[JSON] ()
    , changeNode :: route :- "graph" :> "node" :> Capture "id" Integer :> ReqBody '[JSON] String :> PutNoContent '[JSON] ()
    , putLink :: route :- "graph" :> "link" :> Capture "idFrom" Integer :> Capture "idTo" Integer :> PutNoContent '[JSON] ()
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
serverr pool = return todoSwagger :<|> (hoistServer apiPsql appMToHandler $
  (getResult $ getNodes $ getAllNodesSession)
  :<|> (\i -> getResult $ getNodes $ getNeighboursNodesSession i)
  :<|> (\s -> getResult $ getNodes $ insertNodeSession s)
  :<|> (\i -> getResult $ getNodes $ deleteNodeSession i) 
  :<|> (\i s -> getResult $ getNodes $ renameNodeSession i s)
  :<|> (\i1 i2 -> getResult $ getNodes $ insertLinkSession i1 i2)
  )
  where
    appMToHandler m = runReaderT m pool
    getResult m = do
      result <- m
      case result of
        Right res -> pure res
        Left error -> parseUsageError error
      where
        parseUsageError (ConnectionError (Just msg)) = throw500 msg
        parseUsageError (ConnectionError (Nothing)) = throw500 "Database connection error"
        parseUsageError (SessionError (Session.QueryError _ _ msg)) = throw500 $ BS.pack $ show msg
        throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}
    getR = getResult . getNodes

app :: Pool -> Application
app pool = serve api $ serverr pool

server :: IO ()
server = do
  pool <- acquire settings
  run 8080 $ app pool
  where
    settings = (1, 1, Connection.settings "localhost" (fromInteger 5432) "poster" "password" "")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = LBS.writeFile "example/swagger.json" (encodePretty todoSwagger)
