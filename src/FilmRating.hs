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
