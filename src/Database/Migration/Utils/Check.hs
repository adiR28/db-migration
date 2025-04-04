module Database.Migration.Utils.Check where

import qualified Data.Foldable as DF
import qualified Data.List as DL
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Database.Beam.Migrate as BM
import Database.Migration.Types hiding (mod)
import qualified Database.Migration.Types.LinkedHashMap as LHM
import Database.Migration.Utils.Beam
import qualified Database.Migration.Types as DMT
import qualified Data.HashMap.Strict as HM
import Control.Monad (when,void)
import qualified Debug.Trace as DT
import qualified Database.Beam.Migrate.Types as BMT
import qualified Data.Time as DT
import qualified System.IO.Unsafe as SIU 
import qualified Text.Read as TR

-- type Year = Integer
-- type Month = Int

getCurrentMonthAndYear :: IO (Integer,Int)
getCurrentMonthAndYear = do
  (year,month,_) <- DT.getCurrentTime >>= return . DT.toGregorian . DT.utctDay
  return (year,month)

lenientlyCheckPredicate ::
     Options
  -> DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> Maybe DBPredicate
lenientlyCheckPredicate options@Options{partitionOptions} predicate groupedDBPredicates = do
  case extractQualifiedTableNameFromPredicate predicate of
    Nothing -> lenientPredicateCheck options predicate groupedDBPredicates
    Just qualifiedTblName -> do
      let (tblName,paritionParam) = extractOrigTblNameAndPartitionParam options qualifiedTblName
      case HM.lookup tblName $ DMT.partitionMap partitionOptions of -- check parition options
        Nothing -> lenientPredicateCheck options predicate groupedDBPredicates
        Just _ -> do
          let loopbackLimit = DMT.loopBackLimit partitionOptions
          case HM.lookup tblName $ DMT.partitionedTableMapLimit loopbackLimit of
            Nothing -> handleLoopbackLimit (DMT.defaultLimit loopbackLimit) (tblName,paritionParam) qualifiedTblName options predicate groupedDBPredicates
            Just lbl -> handleLoopbackLimit lbl (tblName,paritionParam) qualifiedTblName options predicate groupedDBPredicates
  where 
    handleLoopbackLimit :: LoopbackLimitValue -> (T.Text,T.Text) -> BMT.QualifiedName -> Options -> DBPredicate -> LHM.LinkedHashMap T.Text DBPredicate -> Maybe DBPredicate
    handleLoopbackLimit NoLimit _  _ opt preds gdb = lenientPredicateCheck opt preds gdb
    handleLoopbackLimit (Limit lmt) (tblName,paritionParam) qualifiedTblName opt preds gdb = do
      let isPartitionedTableBeyondLimit = checkParitionedTableLimit options paritionParam lmt
      if isPartitionedTableBeyondLimit
        then do
          let maybeLenientCheckResp = lenientPredicateCheck opt preds gdb
          case maybeLenientCheckResp of
            Nothing -> Nothing
            Just dbPredicate -> do
              when (DMT.logPreloopbackPartionedErr $ DMT.loopBackLimit $ DMT.partitionOptions opt) $ 
                (return $ DT.traceShow "DB NOT_IN_SYNC " (show (preds,qualifiedTblName))) *> return ()
              Nothing
        else lenientlyCheckPredicate opt preds gdb

    checkParitionedTableLimit :: Options -> T.Text -> Int -> Bool 
    checkParitionedTableLimit opts@Options{partitionOptions} paritionParam lmt = do
      case DMT.partitionFormat partitionOptions  of
        YYYYMM -> do
          let (currYear,currMonth) = SIU.unsafePerformIO getCurrentMonthAndYear
              maybeYear  = TR.readMaybe (T.unpack (T.take 4 paritionParam)) :: Maybe Integer
              maybeMonth = TR.readMaybe (T.unpack (T.drop 4 paritionParam)) :: Maybe Int
              year       = case maybeYear of
                              Just val -> val
                              Nothing -> error "Invalid year"
              month      = case maybeMonth of
                            Just val -> val
                            Nothing -> error "Invalid month"
          checkIfTableIsInRange currYear currMonth year month lmt
        MMYYYY -> do
          let (currYear,currMonth) = SIU.unsafePerformIO getCurrentMonthAndYear
              maybeYear  = TR.readMaybe (T.unpack (T.drop 4 paritionParam)) :: Maybe Integer
              maybeMonth = TR.readMaybe (T.unpack (T.take 2 paritionParam)) :: Maybe Int
              year       = case maybeYear of
                              Just val -> val
                              Nothing -> error "Invalid year"
              month      = case maybeMonth of
                            Just val -> val
                            Nothing -> error "Invalid month"
          checkIfTableIsInRange currYear currMonth year month lmt
      
    checkIfTableIsInRange  :: Integer -> Int -> Integer -> Int -> Int -> Bool
    checkIfTableIsInRange currYear currMonth year month lmt = do
        let prevYear   = currYear - toInteger (lmt `div` 12) - (if (lmt `mod` 12 >= currMonth) then 1 else 0)
        let  prevMonth 
                | lmt `mod` 12 < currMonth = currMonth - (lmt `mod` 12) :: Int
                | otherwise = 12 - (lmt `mod` 12 - currMonth)
        prevYear > year || (prevYear == year && prevMonth >= month)
        
    extractOrigTblNameAndPartitionParam :: Options -> BMT.QualifiedName -> (T.Text,T.Text)
    extractOrigTblNameAndPartitionParam options@Options{partitionOptions} qualifiedTableName = do
      let tblName = BMT.qnameAsText qualifiedTableName
      let partitonTblList = T.splitOn (DMT.partitionDelimiter partitionOptions) tblName
      let tableName = partitonTblList DL.!! 0
      let partition = partitonTblList DL.!! 1
      (tableName, partition)


extractQualifiedTableNameFromPredicate :: DBPredicate -> Maybe BMT.QualifiedName
extractQualifiedTableNameFromPredicate (DBHasEnum _) = Nothing
extractQualifiedTableNameFromPredicate (DBHasSequence _) = Nothing
extractQualifiedTableNameFromPredicate (DBHasTable (TablePredicate (TableInfo tblName) _ _)) = Just tblName
extractQualifiedTableNameFromPredicate (DBTableHasColumns _) = Nothing
extractQualifiedTableNameFromPredicate (DBHasSchema _) = Nothing
extractQualifiedTableNameFromPredicate (DBTableHasIndex tblIndexPredicate@TableHasIndexPredicate{tableName}) = Just tableName

lenientPredicateCheck ::
     Options
  -> DBPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> Maybe DBPredicate
lenientPredicateCheck Options {typeLenient} pd@(DBTableHasColumns colPreds) groupedDBPredicates =
  case typeLenient of
    Nothing -> Just pd
    Just lenientTypeCheck ->
      let lenientPreds =
            DF.foldl'
              (\acc p@ColumnPredicate {columnName} ->
                 if lenientColumnPredicateCheck
                      lenientTypeCheck
                      p
                      groupedDBPredicates
                   then acc
                   else LHM.insert columnName p acc)
              LHM.empty
              colPreds
       in if LHM.null lenientPreds
            then Nothing
            else Just $ DBTableHasColumns lenientPreds
lenientPredicateCheck Options {ignoreEnumOrder} pd@(DBHasEnum (EnumPredicate (EnumInfo enumName enumValues) _ _)) groupedDBPredicates =
  let valuesInDB =
        case LHM.lookup (mkTableName enumName) groupedDBPredicates of
          Just (DBHasEnum (EnumPredicate enumInfo _ _)) -> values enumInfo
          _ignore -> []
   in if ignoreEnumOrder && DL.sort enumValues == DL.sort valuesInDB
        then Nothing
        else Just pd
lenientPredicateCheck Options {ignoreIndexName} pd@(DBTableHasIndex p) groupedDBPredicates =
  if ignoreIndexName && lenientIndexPredicateCheck p groupedDBPredicates
    then Nothing
    else Just pd
lenientPredicateCheck _ p _ = Just p

lenientColumnPredicateCheck ::
     ColumnTypeCheck
  -> ColumnPredicate
  -> LHM.LinkedHashMap T.Text DBPredicate
  -> Bool
lenientColumnPredicateCheck lenientTypeCheck p groupedDBPredicates = do
  let cTable@(BM.QualifiedName _ tblName) = columnTable p
      tableName = mkTableName cTable
      colName = columnName p
  fromMaybe False $ do
    dbP <- LHM.lookup tableName groupedDBPredicates
    case dbP of
      DBHasTable (TablePredicate _ dbColPreds _) -> do
        dbCType <- columnType =<< LHM.lookup colName dbColPreds
        cType <- columnType p
        return $ lenientTypeCheck tblName colName cType dbCType
      _ignore -> Nothing

lenientIndexPredicateCheck ::
     TableHasIndexPredicate -> LHM.LinkedHashMap T.Text DBPredicate -> Bool
lenientIndexPredicateCheck tp gp = isJust $ findIndexInDBPredicates tp gp
