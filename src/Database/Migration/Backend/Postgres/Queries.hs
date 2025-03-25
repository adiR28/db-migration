module Database.Migration.Backend.Postgres.Queries where

import Data.String (fromString)
import qualified Data.Text as T
import qualified Database.Beam.Postgres as BP
import qualified Database.PostgreSQL.Simple as Pg
import Data.Maybe(fromMaybe)
import qualified Data.Vector as V
import Control.Monad (void)
import qualified Database.Beam.Postgres.Migrate as BPM
import Database.Migration.Utils.Common(headMaybe)

getSequencesFromPg ::
     BP.Connection
  -> T.Text
  -> IO [(String, String, String, String, String, String, String, String)]
getSequencesFromPg conn mSchema =
    BPM.executePgQueryAndWrap conn
      (fromString
        $ unlines
            [ "select sequence_schema, sequence_name, minimum_value,"
            , "maximum_value, start_value, increment, cycle_option, data_type"
            , "from information_schema.sequences"
            , "where sequence_schema = '" ++ T.unpack mSchema ++ "';"
            ])
      BPM.mkToRowInstanceMaybe

getSchemasFromPg :: BP.Connection -> IO [T.Text]
getSchemasFromPg conn =
  map Pg.fromOnly <$> BPM.executePgQueryAndWrap conn 
    (fromString
        "select schema_name from information_schema.schemata where catalog_name = current_database();")
    BPM.mkToRowInstanceMaybe

getColumnDefaultsFromPg ::
     BP.Connection -> T.Text -> IO [(T.Text, T.Text, T.Text, T.Text)]
getColumnDefaultsFromPg conn mSchema =
    BPM.executePgQueryAndWrap 
      conn
      (fromString
          $ unlines
              [ "select table_schema, table_name, column_name, column_default"
              , "from information_schema.columns where"
              , "column_default is not null and"
              , "table_schema = '" ++ T.unpack mSchema ++ "';"
              ])
      BPM.mkToRowInstanceMaybe

getSearchPath :: BP.Connection -> IO [T.Text]
getSearchPath conn = 
  fromMaybe [] . headMaybe . fmap (V.toList . Pg.fromOnly)
    <$> BPM.executePgQueryAndWrap conn (fromString "select current_schemas(false)") BPM.mkToRowInstanceMaybe

setSearchPath :: BP.Connection -> [T.Text] -> IO ()
setSearchPath conn schemas =
  void
    $ Pg.execute_
        conn
        (fromString
           $ "set search_path = '"
               ++ T.unpack (T.intercalate "', '" schemas)
               ++ "';")
