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
import           Data.Maybe                                            (fromMaybe)
import           Data.Monoid                                           ((<>))
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


type BigQueryScope =
  '[ "https://www.googleapis.com/auth/bigquery"
   , "https://www.googleapis.com/auth/cloud-platform"
   , "https://www.googleapis.com/auth/cloud-platform.read-only" ]

extractRowsAsJson
  :: Lens' a [TableRow]
  -> a
  -> [[Maybe Value]]
extractRowsAsJson tableRowsExtractionLens =
  fmap (fmap (view tcV)) . toListOf (tableRowsExtractionLens . traverse . trF)

-- | Pages results from the BigQuery API lazily building a list of
--   JSON results, where a row = [Maybe Value]
stdSqlRequest
  :: ( MonadGoogle BigQueryScope m,
       HasScope BigQueryScope JobsQuery)
  => Text
  -- ^ Project ID, i.e. `parsley-data`
  -> Text
  -- ^ Query SQL
  -> m [[Maybe Value]]
stdSqlRequest projectId sql = do
  initRes <- send (jobsQuery (mkStdSqlRequest sql) projectId)
  let initRows = extractRowsAsJson qRows initRes
  case (,) <$> initRes^.qJobReference <*> initRes^.qPageToken of
    Just (jr, pt) -> (<>) initRows <$> iter jr (Just pt)
    Nothing       -> pure initRows
  where iter jr pt = do
          res <- send (jobsGetQueryResults (jr^.jrJobId.non "") (jr^.jrProjectId.non "") & jgqrPageToken .~ pt)
          case res^.gqrrPageToken of
            Just pt' -> (<>) (extractRowsAsJson gqrrRows res) <$> iter jr (Just pt')
            Nothing  -> return $ extractRowsAsJson gqrrRows res

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
  -> IO [[Maybe Value]]
testQuery projectName query = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy :: Proxy BigQueryScope))
  runResourceT . runGoogle env $ stdSqlRequest projectName query


-- * Generic Product Type Parsing -----------------------------------------------

class BigQueryColumn a where
  parseCol :: Maybe Value -> Result a

parseBigQueryColumns
  :: ( Generic a
     , Code a ~ '[xs]
     , All BigQueryColumn xs )
  => [Maybe Value]
  -> Result a
parseBigQueryColumns vs =
  fromMaybe (fail ("Unexpected column data in row: " ++ show vs)) $
   fmap (to . SOP . Z) <$>
    hctraverse (Proxy @BigQueryColumn) (parseCol . unK) <$>
     fromList vs
