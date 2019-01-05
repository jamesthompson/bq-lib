{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module BQ.Effects where

import           BQ.Types.BigQuery                                     (BigQueryRow)
import           Control.Lens                                          (non,
                                                                        to,
                                                                        toListOf,
                                                                        traverse,
                                                                        view,
                                                                        (&),
                                                                        (.~),
                                                                        (<&>),
                                                                        (?~),
                                                                        (^.))
import           Data.Machine                                          hiding
                                                                        (Z)
import           Data.Proxy                                            (Proxy (..))
import           Data.Text                                             (Text)
import           Network.Google                                        (HasScope,
                                                                        LogLevel (Debug),
                                                                        MonadGoogle,
                                                                        envLogger,
                                                                        envScopes,
                                                                        newEnv,
                                                                        newLogger,
                                                                        runGoogle,
                                                                        runResourceT,
                                                                        send)
import           Network.Google.BigQuery.Types                         (gqrrPageToken,
                                                                        gqrrRows,
                                                                        jrJobId,
                                                                        jrProjectId,
                                                                        qJobReference,
                                                                        qPageToken,
                                                                        qRows,
                                                                        qrQuery,
                                                                        qrUseLegacySQL,
                                                                        qrUseQueryCache,
                                                                        queryRequest,
                                                                        tcV,
                                                                        trF)
import           Network.Google.Resource.BigQuery.Jobs.GetQueryResults (jgqrPageToken,
                                                                        jobsGetQueryResults)
import           Network.Google.Resource.BigQuery.Jobs.Query           (JobsQuery,
                                                                        jobsQuery)
import           System.IO                                             (stdout)


-- * Gogol / BigQuery Machinery -------------------------------------------------

-- | Gogol Scope
type BigQueryScope =
  '[ "https://www.googleapis.com/auth/bigquery"
   , "https://www.googleapis.com/auth/cloud-platform"
   , "https://www.googleapis.com/auth/cloud-platform.read-only" ]

-- | Machines source for the given project id and query from the BigQuery API
stdSqlRequest
  :: ( MonadGoogle BigQueryScope m,
       HasScope BigQueryScope JobsQuery)
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
          res <- send (jobsGetQueryResults (jr^.jrJobId.non "") (jr^.jrProjectId.non "") & jgqrPageToken .~ pt)
          pure $ Yield (res^.bqRows gqrrRows) (iter (Just jr) (res^.gqrrPageToken))
        iter _         _           = MachineT $ pure Stop

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

