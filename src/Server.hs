{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Server
  ( server
  ) where

import Control.Monad.Reader
import Data.Swagger
import qualified Data.ByteString.Lazy as LBS
import Data.Profunctor
import GHC.Generics (Generic)
import qualified Hasql.Connection as Connection
import Hasql.Pool
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Data.Aeson.Encode.Pretty (encodePretty)
import Servant.API.Generic

import NodeData
import NodeDao

data Routes route = Routes
    { getNodesApi :: route :- "graph" :> "node" :> Get '[JSON] [Node]
    , getNeighboursNodesApi :: route :- "graph" :> "node" :> Capture "id" Integer :> "neighbours" :> Get '[JSON] [Node]
    , putNodeApi :: route :- "graph" :> "node" :> ReqBody '[JSON] NodeLabel :> Put '[JSON] Int
    , deleteNodeApi :: route :- "graph" :> "node" :> Capture "id" Integer :> DeleteNoContent '[JSON] ()
    , changeNodeApi :: route :- "graph" :> "node" :> Capture "id" Integer :> ReqBody '[JSON] NodeLabel :> PutNoContent '[JSON] ()
    , putLinkApi :: route :- "graph" :> "link" :> Capture "idFrom" Integer :> Capture "idTo" Integer :> PutNoContent '[JSON] ()
    }
  deriving (Generic)

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type AllApi = SwaggerAPI :<|> (ToServantApi Routes)

todoSwagger :: Swagger
todoSwagger = toSwagger api

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

allapi :: Proxy AllApi
allapi = Proxy

serverr :: Pool -> Server AllApi
serverr pool = return todoSwagger :<|> (hoistServer api appMToHandler
  (((nodesDao getAllNodes)
  :<|> ((\i -> nodesDao (getNeighboursNodes i))
  :<|> (\(NodeLabel s) -> nodesDao (insertNode s))))
  :<|> ((\i -> nodesDao (deleteNode i))
  :<|> ((\i (NodeLabel s) -> nodesDao (renameNode i s))
  :<|> (\i1 i2 -> nodesDao (insertLink i1 i2))))))
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
