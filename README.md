# bq-lib

A library for working with `gogol-BigQuery` (@ v0.3.0) to query (including paging) for results using `machines` and parsing returned rows directly into product types (i.e. datatypes with a single record constructor) using `generics-sop`.

Also contains a non-empty text datatype with a "smart" prism constructor for use as I'm always running into empty string fields when they should really be null.

## Development

```sh
$ nix-shell -A shell
> cabal new-repl lib
```

## Usage

Brief pseudo-code to explain the general usage pattern of the library:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson             (Result)               -- from `aeson`
import           BQ.Effects             (testDebugQuery)       -- from `bq-lib`
import           BQ.Parsing             (parseBigQueryColumns) -- from `bq-lib`
import           BQ.Types.Text1         (Text1)                -- from `bq-lib`
import qualified GHC.Generics as GHC                           -- from GHC
import           Generics.SOP           (Generic)              -- from `generics-sop`


data ExampleRecord
  = ExampleRecord
  { foo :: Text1
  , bar :: Maybe Text1
  , baz :: Maybe Text1
  } deriving (Show, GHC.Generic)

instance Generic ExampleRecord

result = traverse parseBigQueryColumns
                  (testDebugQuery "$project-id"
                                  "select 'hello' as foo, '' as empty_text1_bar, cast(null as String) as baz"
-- result = Success [ExampleRecord { foo : 'hello', bar : Nothing, baz : Nothing }]
```
