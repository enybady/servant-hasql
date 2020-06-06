module NodeData (
  Node (..)
  ) where

import Data.Aeson
import Data.Swagger
import GHC.Generics (Generic)

data Node =
  Node
    { nodeId :: Int
    , label  :: String
    } deriving (Eq, Show, Generic)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> Node 
    <$> v .: "id"
    <*> v .: "label"

instance ToJSON Node where
  toJSON (Node i l) = object [ "id" .= i, "label" .= l]

instance ToSchema Node