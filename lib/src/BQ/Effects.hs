{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module BQ.Effects where

import           BQ.Types.BigQuery                                     (BigQueryRow)
import           Control.Lens                                          (Lens',
                                                                        non,
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
import           Network.Google.BigQuery.Types                         (QueryRequest,
                                                                        TableRow,
                                                                        gqrrPageToken,
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

extractRowsAsJson
  :: Lens' a [TableRow]
  -> a
  -> [BigQueryRow]
extractRowsAsJson tableRowsExtractionLens =
  fmap (fmap (view tcV)) . toListOf (tableRowsExtractionLens . traverse . trF)

-- | Machines source for for the given project id and query from the BigQuery API
stdSqlRequest
  :: ( MonadGoogle BigQueryScope m,
       HasScope BigQueryScope JobsQuery)
  => Text
  -- ^ GCP Project Name
  -> Text
  -- ^ Query SQL
  -> SourceT m BigQueryRow
stdSqlRequest projectId sql = (flattened <~) . MachineT $ do
  initRes <- send (jobsQuery (mkStdSqlRequest sql) projectId)
  let initRows = extractRowsAsJson qRows initRes
  pure $ Yield initRows (iter (initRes^.qJobReference) (initRes^.qPageToken))
  where iter (Just jr) pt@(Just _) = MachineT $ do
          res <- send (jobsGetQueryResults (jr^.jrJobId.non "") (jr^.jrProjectId.non "") & jgqrPageToken .~ pt)
          pure $ Yield (extractRowsAsJson gqrrRows res) (iter (Just jr) (res^.gqrrPageToken))
        iter _         _           = MachineT $ pure Stop

-- | Build a standard SQL request for use in stdSqlRequest
mkStdSqlRequest
  :: Text
  -- ^ Query SQL
  -> QueryRequest
mkStdSqlRequest sql =
  queryRequest & qrUseQueryCache .~ True
               & qrUseLegacySQL  .~ False
               & qrQuery         ?~ sql

-- | Utility function to test queries
testQuery
  :: Text
    -- ^ GCP Project Name
  -> Text
    -- ^ Query SQL
  -> IO [BigQueryRow]
testQuery projectName query = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy :: Proxy BigQueryScope))
  runResourceT . runGoogle env . runT $ stdSqlRequest projectName query

