module BQ.Types.BigQuery where

import           Data.Aeson (Value)

-- | A BigQueryRow is a list of Maybe Value in Gogol

type BigQueryRow = [Maybe Value]
