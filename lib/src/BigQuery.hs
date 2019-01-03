{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import           Data.Aeson                                            (Value)
import           Data.Monoid                                           ((<>))
import           Data.Proxy
import           Data.Text                                             (Text)
import           Network.Google                                        (HasScope,
                                                                        LogLevel (Info),
                                                                        MonadGoogle,
                                                                        envLogger,
                                                                        envScopes,
                                                                        newEnv,
                                                                        newLogger,
                                                                        runGoogle,
                                                                        runResourceT,
                                                                        send)
import           Network.Google.BigQuery.Types
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

-- | Utility function to test queries and parsers
testQuery
  :: Text
  -> IO [[Maybe Value]]
testQuery query = do
  lgr <- newLogger Info stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ (Proxy :: Proxy BigQueryScope))
  runResourceT . runGoogle env $ stdSqlRequest "parsley-data" query

class BigQueryColumn a where
  parseCol :: Maybe Value -> Result a

parseBigQueryColumns
  :: ( Generic a
     , Code a ~ '[xs]
     , All BigQueryColumn xs )
  => [Maybe Value]
  -> Result a
parseBigQueryColumns vs =
  fromMaybe (fail ("Unexpected number of columns in row: " ++ show vs)) $
   fmap (to . SOP . Z) <$>
    hctraverse (Proxy @BigQueryColumn) (parseCol . unK) <$>
     fromList vs
