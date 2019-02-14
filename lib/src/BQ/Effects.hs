{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module BQ.Effects where

import           BQ.Types.BigQuery                                     (BigQueryRow)
import           Control.Lens.Fold                                     (toListOf)
import           Control.Lens.Getter                                   (to,
                                                                        view,
                                                                        (^.))
import           Control.Lens.Iso                                      (non)
import           Control.Lens.Setter                                   ((.~),
                                                                        (?~))
import           Control.Monad.IO.Class                                (liftIO)
import           Data.Aeson                                            (Value)
import           Data.Function                                         ((&))
import           Data.Functor                                          ((<&>))
import qualified Data.HashMap.Lazy                                     as HM
import           Data.Machine                                          hiding
                                                                        (Z)
import           Data.Proxy                                            (Proxy (..))
import           Data.Text                                             (Text)
import           Data.Traversable                                      (traverse)
import           Data.UUID                                             (toText)
import           Data.UUID.V4                                          (nextRandom)
import           Network.Google                                        (Google,
                                                                        HasScope,
                                                                        LogLevel (Debug),
                                                                        MonadGoogle,
                                                                        envLogger,
                                                                        envScopes,
                                                                        newEnv,
                                                                        newLogger,
                                                                        runGoogle,
                                                                        runResourceT,
                                                                        send,
                                                                        timeout)
import           Network.Google.BigQuery.Types                         (gqrrPageToken,
                                                                        gqrrRows,
                                                                        jrJobId,
                                                                        jrProjectId,
                                                                        jsonObject,
                                                                        qJobReference,
                                                                        qPageToken,
                                                                        qRows,
                                                                        qrQuery,
                                                                        qrUseLegacySQL,
                                                                        qrUseQueryCache,
                                                                        queryRequest,
                                                                        tableDataInsertAllRequest,
                                                                        tableDataInsertAllRequestRowsItem,
                                                                        tcV,
                                                                        tdiarRows,
                                                                        tdiarriInsertId,
                                                                        tdiarriJSON,
                                                                        trF)
import           Network.Google.Resource.BigQuery.Jobs.GetQueryResults (jgqrPageToken,
                                                                        jobsGetQueryResults)
import           Network.Google.Resource.BigQuery.Jobs.Query           (JobsQuery,
                                                                        jobsQuery)
import           Network.Google.Resource.BigQuery.TableData.InsertAll  (TableDataInsertAll,
                                                                        tableDataInsertAll)
import           System.IO                                             (stdout)


-- * Gogol / BigQuery Machinery -------------------------------------------------

-- | Gogol Scope
type BigQueryScope =
  '[ "https://www.googleapis.com/auth/bigquery"
   , "https://www.googleapis.com/auth/bigquery.insertdata"
   , "https://www.googleapis.com/auth/cloud-platform"
   , "https://www.googleapis.com/auth/cloud-platform.read-only" ]

-- | Machines source for the given project id and query from the BigQuery API
stdSqlRequest
  :: ( MonadGoogle BigQueryScope m,
       HasScope BigQueryScope JobsQuery )
  => Text
  -- ^ GCP Project Id
  -> Text
  -- ^ Query SQL
  -> SourceT m BigQueryRow
stdSqlRequest projectId sql = (flattened <~) . MachineT $ do
  initRes <- send (jobsQuery assembledQueryReq projectId)
  let initRows = initRes^.bqRows qRows
  pure $ Yield initRows (iter (initRes^.qJobReference) (initRes^.qPageToken))
  where assembledQueryReq =
          queryRequest & qrUseQueryCache .~ True
                       & qrUseLegacySQL  .~ False
                       & qrQuery         ?~ sql
        bqRows tableRowsGetter =
          to $ fmap (fmap (view tcV)) . toListOf (tableRowsGetter . traverse . trF)
        iter (Just jr) pt@(Just _) = MachineT $ do
          res <- send (jobsGetQueryResults (jr^.jrJobId.non "")
                                           (jr^.jrProjectId.non "") &
                       jgqrPageToken .~ pt)
          pure $ Yield (res^.bqRows gqrrRows) (iter (Just jr) (res^.gqrrPageToken))
        iter _         _           = MachineT $ pure Stop

writeRowsToTable
  :: ( MonadGoogle BigQueryScope m,
       HasScope BigQueryScope TableDataInsertAll )
  => (a -> HM.HashMap Text Value)
  -- ^ Row serialization function
  -> Text
  -- ^ Project ID
  -> Text
  -- ^ Dataset ID
  -> Text
  -- ^ Table ID
  -> MachineT m (Is [a]) ()
writeRowsToTable toBQRow projectId datasetId tableId = autoM $ \rows -> do
  items <- liftIO $ traverse mkRow rows
  let req = tableDataInsertAll (tableDataInsertAllRequest & tdiarRows .~ items)
                               datasetId
                               projectId
                               tableId
  res <- send req
  liftIO $ print res
  return ()
  where mkRow a = (\u -> tableDataInsertAllRequestRowsItem & tdiarriJSON ?~ jsonObject (toBQRow a)
                                                           & tdiarriInsertId ?~ toText u) <$> nextRandom

-- | Utility function to test queries with debug logging in IO
testDebugQuery
  :: Text
    -- ^ GCP Project Id
  -> Text
    -- ^ Query SQL
  -> IO [BigQueryRow]
testDebugQuery projectId sql = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy @BigQueryScope))
  runResourceT . runGoogle env . runT $ stdSqlRequest projectId sql

-- | Runs any machine outputting a list - logging at debug level
runQueryMachine
  :: MachineT (Google BigQueryScope) (Is a) b
  -> IO [b]
runQueryMachine machine = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy @BigQueryScope))
  runResourceT. runGoogle env . timeout 300 $ runT machine

