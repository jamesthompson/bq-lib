{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module BigQuery where

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
import           Data.Aeson                                            (Result,
                                                                        Value)
import           Data.Machine                                          hiding
                                                                        (Z)
import           Data.Maybe                                            (fromMaybe)
import           Data.Proxy                                            (Proxy (..))
import           Data.Text                                             (Text)
import           Generics.SOP                                          (All,
                                                                        Code,
                                                                        Generic,
                                                                        NS (..),
                                                                        SOP (..),
                                                                        fromList,
                                                                        hctraverse,
                                                                        to, unK)
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

-- | A BigQueryRow is a list of Maybe Value in Gogol
type BigQueryRow = [Maybe Value]

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
  return $ Yield initRows (iter (initRes^.qJobReference) (initRes^.qPageToken))
  where iter (Just jr) pt@(Just _) = MachineT $ do
          res <- send (jobsGetQueryResults (jr^.jrJobId.non "") (jr^.jrProjectId.non "") & jgqrPageToken .~ pt)
          return $ Yield (extractRowsAsJson gqrrRows res) (iter (Just jr) (res^.gqrrPageToken))
        iter _         _           = MachineT $ return Stop

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


-- * Generic BigQuery Row to Product Type Parsing -------------------------------

class BigQueryColumn a where
  parseCol :: Maybe Value -> Result a

parseBigQueryColumns
  :: ( Generic a
     , Code a ~ '[xs]
     , All BigQueryColumn xs )
  => BigQueryRow
  -> Result a
parseBigQueryColumns vs =
  fromMaybe (fail ("Unexpected column data in row: " ++ show vs)) $
   fmap (to . SOP . Z) <$>
    hctraverse (Proxy @BigQueryColumn) (parseCol . unK) <$>
     fromList vs

