{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module FilmRating where

import Database.Persist.TH
import Prelude
import GHC.Generics
import Data.Aeson

data FilmRating = G | PG | PG_13 | R | NC_17
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "FilmRating"

instance FromJSON FilmRating
instance ToJSON FilmRating
-- instance FromJSON FilmRating where
--   parseJSON (String "G") = return G
--   parseJSON (String "PG") = return PG
--   parseJSON (String "PG-13") = return PG_13
--   parseJSON (String "R") = return R
--   parseJSON (String "NC-17") = return NC_17
