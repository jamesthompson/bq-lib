{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BQ.Types.Text1
  ( Text1
  , _Text1
  ) where

import           BQ.Parsing         (BigQueryColumnParser (..))
import           Control.Lens.Fold  (preview)
import           Control.Lens.Prism (Prism', prism')
import           Control.Monad.Fail (MonadFail)
import           Data.Aeson         (FromJSON (..), Value (Null, String))
import           Data.Maybe         (maybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)


data Text1
  = Text1 Char Text
  deriving (Eq, Generic, Ord)

instance Show Text1 where
  show (Text1 h t) = show (T.cons h t)

_Text1 :: Prism' Text Text1
_Text1 = prism' fro to'
  where fro (Text1 h t) = T.cons h t
        to' = (fmap (uncurry Text1) . T.uncons)

text1NullError :: MonadFail m => m a
text1NullError =
  fail "Expecting non-null Text1 value"

text1TypeError :: MonadFail m => m a
text1TypeError =
  fail "Expecting a field : String representing a Text1 value"

instance FromJSON Text1 where
  parseJSON (String t) =
    maybe text1NullError pure (preview _Text1 t)
  parseJSON _          = text1TypeError

instance {-# OVERLAPPING #-} FromJSON (Maybe Text1) where
  parseJSON (String t) =
    maybe (pure Nothing) (pure . Just) (preview _Text1 t)
  parseJSON Null       = pure Nothing
  parseJSON _          = text1TypeError

instance MonadFail m => BigQueryColumnParser m Text1 where
  parseCol (Just (String t)) =
    maybe text1NullError pure (preview _Text1 t)
  parseCol (Just _)          = fail text1TypeError
  parseCol Nothing           = fail text1TypeError

instance MonadFail m => BigQueryColumnParser m (Maybe Text1) where
  parseCol (Just (String t)) =
    maybe (pure Nothing) (pure . Just) (preview _Text1 t)
  parseCol (Just Null)       = pure Nothing
  parseCol (Just _)          = fail text1TypeError
  parseCol Nothing           = pure Nothing

