module NodeDao where

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
import Data.Functor.Contravariant
import Data.ByteString.Char8 (append)
import NodeData
import Servant
import Control.Monad.Error.Class (MonadError)
import qualified Hasql.Session as Session
import qualified Data.ByteString.Lazy as LBS

nodesDao :: (MonadIO m, MonadReader Pool m, MonadError ServerError m) => Session a -> m a
nodesDao session = do
  pool <- ask
  result <- liftIO $ use pool session
  getResult result
    where
    getResult (Right a) = pure a
    getResult (Left e)  = parseUsageError e
    parseUsageError (ConnectionError (Just msg)) = throw500 msg
    parseUsageError (ConnectionError (Nothing)) = throw500 "Database connection error"
    parseUsageError (SessionError (Session.QueryError _ _ msg)) = throw500 $ BS.pack $ show msg
    throw500 msg = throwError err500 {errBody = LBS.fromStrict msg}


insertLink :: Integer -> Integer -> Session ()
insertLink i j = statement (fromInteger i, fromInteger j) insertLinkStatement
  where 
    insertLinkStatement = 
      Hasql.Statement.Statement
        "insert into links (idfrom, idto) values ($1, $2);"
        ((fst >$< Encoders.param (Encoders.nonNullable Encoders.int2)) <>
                  (snd >$< Encoders.param (Encoders.nonNullable Encoders.int2)))
        Decoders.noResult
        True
        
getAllNodes :: Session [Node]
getAllNodes = statement () selectNodesStatement
  where
    selectNodesStatement =
      Hasql.Statement.Statement
        "SELECT id, label FROM nodes"
        mempty
        nodeDecoder
        True

getNeighboursNodes :: Integer -> Session [Node]
getNeighboursNodes i = statement (fromInteger i) selectNeighboursStatement
  where
    selectNeighboursStatement =
      Hasql.Statement.Statement
        ("select nodes.id, nodes.label from nodes as nodes " `BS.append`
        "inner join (select idto as id from links where idfrom = $1 union " `BS.append`
        "select idfrom as id from links where idto = $1) as ids on ids.id = nodes.id;")
      (Encoders.param $ Encoders.nonNullable Encoders.int2)
      nodeDecoder
      True

insertNode :: String -> Session Int
insertNode label = statement (T.pack label) insertNodeStatement
  where
    insertNodeStatement =
      Hasql.Statement.Statement
      "INSERT into nodes(label) values ($1) returning id;"
      (Encoders.param $ Encoders.nonNullable $ Encoders.text)
      (Decoders.singleRow $ Decoders.column $ Decoders.nonNullable $ fromEnum <$> Decoders.int2)
      True
      
renameNode :: Integer -> String -> Session ()
renameNode i label = statement (fromInteger i, T.pack label) renameNodeStatement
  where
    renameNodeStatement =
      Hasql.Statement.Statement
      "INSERT into nodes(label) values ('$1')"
      ((fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <>
          (snd >$< Encoders.param (Encoders.nonNullable Encoders.text)))
      Decoders.noResult
      True

deleteNode :: Integer -> Session ()
deleteNode i = statement (fromInteger i) insertNodeStatement
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