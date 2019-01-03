{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module BQ.Parsing where

import           BQ.Types.BigQuery (BigQueryRow)
import           Data.Aeson        (Result (..), Value)
import           Data.Machine      (Is, MachineT, await, construct, stop, yield)
import           Data.Maybe        (fromMaybe)
import           Data.Proxy        (Proxy (..))
import           Generics.SOP      (All, Code, Generic, NS (..), SOP (..),
                                    fromList, hctraverse, to, unK)


-- * Generic BigQuery Row to Product Type Parsing -------------------------------

class BigQueryColumnParser a where
  parseCol :: Maybe Value -> Result a

parseBigQueryColumns
  :: ( Generic a
     , Code a ~ '[xs]
     , All BigQueryColumnParser xs )
  => BigQueryRow
  -> Result a
parseBigQueryColumns vs =
  fromMaybe (fail ("Unexpected column data in row: " ++ show vs)) $
   fmap (to . SOP . Z) <$>
    hctraverse (Proxy @BigQueryColumnParser) (parseCol . unK) <$>
     fromList vs

parseRows
  :: ( Generic a
     , Code a ~ '[xs]
     , All BigQueryColumnParser xs
     , Monad m )
  => MachineT m (Is BigQueryRow) a
parseRows = construct $ do
  bqr <- await
  case parseBigQueryColumns bqr of
    Success x -> yield x
    Error   _ -> stop

