module Orphans where

import Data.CaseInsensitive  (CI, FoldCase, mk, original)
import Data.Aeson            (FromJSON(..), ToJSON(..))


instance (FoldCase a, FromJSON a) => FromJSON (CI a) where
  parseJSON a = mk <$> parseJSON a

instance ToJSON a => ToJSON (CI a) where
  toJSON = toJSON . original
