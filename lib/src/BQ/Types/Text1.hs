{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BQ.Types.Text1
  ( Text1
  , _Text1
  ) where

import           BQ.Parsing          (BigQueryColumnParser (..))
import           Control.Lens.Fold   (preview)
import           Control.Lens.Prism  (Prism', prism')
import           Control.Lens.Review (review)
import           Control.Monad.Fail  (MonadFail)
import           Data.Aeson          (FromJSON, ToJSON, Value (Null, String),
                                      parseJSON, toJSON)
import           Data.Maybe          (maybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)


data Text1
  = Text1 Char Text
  deriving (Eq, Generic, Ord)

instance Show Text1 where
  show (Text1 h t) = show (T.cons h t)

_Text1 :: Prism' Text Text1
_Text1 = prism' fro to'
  where fro (Text1 h t) = T.cons h t
        to' = (fmap (uncurry Text1) . T.uncons)

text1ParseError :: MonadFail m => m a
text1ParseError =
  fail "Expecting non-empty Text1 value"

instance FromJSON Text1 where
  parseJSON (String t) =
    maybe text1ParseError pure (preview _Text1 t)
  parseJSON _          = text1ParseError

instance {-# OVERLAPPING #-} FromJSON (Maybe Text1) where
  parseJSON (String t) =
    maybe (pure Nothing) (pure . Just) (preview _Text1 t)
  parseJSON Null       = pure Nothing
  parseJSON _          = text1ParseError

instance ToJSON Text1 where
  toJSON = String . review _Text1

text1BigQueryNullError :: MonadFail m => m a
text1BigQueryNullError =
  fail "Expecting non-null Text1 value"

text1BigQueryTypeError :: MonadFail m => m a
text1BigQueryTypeError =
  fail "Expecting a field : String representing a Text1 value"

instance MonadFail m => BigQueryColumnParser m Text1 where
  parseCol (Just (String t)) =
    maybe text1BigQueryNullError pure (preview _Text1 t)
  parseCol (Just _)          =
    fail text1BigQueryTypeError
  parseCol Nothing           =
    fail text1BigQueryNullError

instance MonadFail m => BigQueryColumnParser m (Maybe Text1) where
  parseCol (Just (String t)) =
    maybe (pure Nothing) (pure . Just) (preview _Text1 t)
  parseCol (Just Null)       = pure Nothing
  parseCol (Just _)          = fail text1BigQueryTypeError
  parseCol Nothing           = pure Nothing

