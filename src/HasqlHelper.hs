module HasqlHelper where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Pool
import Hasql.Session (Session, statement)
import Hasql.Statement
import Data.Int (Int16)
import Data.ByteString.Char8 (append)
import NodeData

getNodes :: (MonadIO m, MonadReader Pool m) => Session a -> m (Either UsageError a)
getNodes session = do
  pool <- ask
  liftIO $ use pool session

selectAllNodesSession :: Session [Node]
selectAllNodesSession = statement () selectNodesStatement
  where
    selectNodesStatement =
      Hasql.Statement.Statement
        "SELECT id, label FROM nodes"
        mempty
        nodeDecoder
        True

selectNeighboursNodesSession :: Integer -> Session [Node]
selectNeighboursNodesSession i = statement (fromInteger i) selectNeighboursStatement
  where
    selectNeighboursStatement =
      Hasql.Statement.Statement
        ("select nodes.id, nodes.label from nodes as nodes " `BS.append`
        "inner join (select idto as id from links where idfrom = $1 union " `BS.append`
        "select idfrom as id from links where idto = $1) as ids on ids.id = nodes.id;")
      (Encoders.param $ Encoders.nonNullable Encoders.int2)
      nodeDecoder
      True

insertNodeSession :: String -> Session Int
insertNodeSession label = statement (T.pack label) insertNodeStatement
  where
    insertNodeStatement =
      Hasql.Statement.Statement
      "INSERT into nodes(label) values ('$1')"
      (Encoders.param $ Encoders.nonNullable $ Encoders.text)
      (Decoders.singleRow $ Decoders.column $ Decoders.nonNullable $ fromEnum <$> Decoders.int2)
      True
      
{-renameNodeSession :: Integer -> String -> Session Int
renameNodeSession i label = statement (fromInteger i, T.pack label) insertNodeStatement
  where
    insertNodeStatement =
      Hasql.Statement.Statement
      "INSERT into nodes(label) values ('$1')"
      (fst >$< Encoders.param $ Encoders.nonNullable $ Encoders.text)
      (Decoders.singleRow $ Decoders.column $ Decoders.nonNullable $ fromEnum <$> Decoders.int2)
      True
-}
deleteNodeSession :: Integer -> Session ()
deleteNodeSession i = statement (fromInteger i) insertNodeStatement
  where
    insertNodeStatement =
      Hasql.Statement.Statement
      "delete from nodes where id = $1"
      (Encoders.param $ Encoders.nonNullable Encoders.int2)
      Decoders.noResult
      True


nodeDecoder :: Decoders.Result [Node]
nodeDecoder = Decoders.rowList $ Node
   <$> (Decoders.column $ Decoders.nonNullable $ fromEnum <$> Decoders.int2)
   <*> (Decoders.column $ Decoders.nonNullable $ T.unpack <$> Decoders.text)