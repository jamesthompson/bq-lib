{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module BQ.Parsing where

import           BQ.Types.BigQuery  (BigQueryRow)
import           Control.Monad.Fail (MonadFail)
import           Data.Aeson         (Result (..), Value)
import           Data.Machine       (Is, MachineT, await, construct, stop,
                                     yield)
import           Data.Maybe         (fromMaybe)
import           Data.Proxy         (Proxy (..))
import           Generics.SOP       (All, Code, Generic, NS (..), SOP (..),
                                     fromList, hctraverse, to, unK)


-- * Generic BigQuery Row to Product Type Parsing -------------------------------

-- | A class for parsing BigQuery row fields
class MonadFail f => BigQueryColumnParser f a where
  parseCol :: Maybe Value -> f a

-- | Parse any given generic record
parseBigQueryColumns
  :: forall a f xs .
     ( Generic a
     , Code a ~ '[xs]
     , MonadFail f
     , All (BigQueryColumnParser f) xs )
  => BigQueryRow
  -> f a
parseBigQueryColumns vs =
  fromMaybe (fail ("Unexpected column data in row: " ++ show vs)) $
   fmap (to . SOP . Z) <$>
    hctraverse (Proxy :: Proxy (BigQueryColumnParser f)) (parseCol . unK) <$>
     fromList vs

-- | Row parsing machine, stops if a failing row is encountered
--   Requires `Result` parameterized BigQueryColumnParser instances
parseRows
  :: ( Generic a
     , Code a ~ '[xs]
     , All (BigQueryColumnParser Result) xs
     , Monad m )
  => MachineT m (Is BigQueryRow) a
parseRows = construct $ do
  bqr <- await
  case parseBigQueryColumns bqr of
    Success x -> yield x
    Error   _ -> stop

