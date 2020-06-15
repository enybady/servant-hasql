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

type Api = "graph" :> "node" :> Get '[JSON] [Node]
       :<|> "graph" :> "node" :> Capture "id" Integer :> "neighbours" :> Get '[JSON] [Node]
       :<|> "graph" :> "node" :> ReqBody '[JSON] String :> Put '[JSON] Int
       :<|> "graph" :> "node" :> Capture "id" Integer :> DeleteNoContent '[JSON] ()
       :<|> "graph" :> "node" :> Capture "id" Integer :> ReqBody '[JSON] String :> PutNoContent '[JSON] ()
       :<|> "graph" :> "link" :> Capture "idFrom" Integer :> Capture "idTo" Integer :> PutNoContent '[JSON] ()

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type AllApi = SwaggerAPI :<|> Api

todoSwagger :: Swagger
todoSwagger = toSwagger api

api :: Proxy Api
api = Proxy

allapi :: Proxy AllApi
allapi = Proxy

serverr :: Pool -> Server AllApi
serverr pool = return todoSwagger :<|> (hoistServer api appMToHandler
  ((getNodes getAllNodesSession)
  :<|> (\i -> getNodes (getNeighboursNodesSession i))
  :<|> (\s -> getNodes (insertNodeSession s))
  :<|> (\i -> getNodes (deleteNodeSession i))
  :<|> (\i s -> getNodes (renameNodeSession i s))
  :<|> (\i1 i2 -> getNodes (insertLinkSession i1 i2))
  ))
  where
    appMToHandler m = runReaderT m pool

app :: Pool -> Application
app pool = serve allapi $ serverr pool

server :: IO ()
server = do
  pool <- acquire settings
  run 8080 $ app pool
  where
    settings = (1, 1, Connection.settings "localhost" (fromInteger 5432) "poster" "password" "")

writeSwaggerJSON :: IO ()
writeSwaggerJSON = LBS.writeFile "example/swagger.json" (encodePretty todoSwagger)
