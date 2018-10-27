{-# LANGUAGE OverloadedStrings #-}

module TestFactories (getTestMovie) where

import TestImport
import FilmRating
import qualified Text.Read as TR

getTestMovie :: Text -> Movie
getTestMovie imdbId = Movie {
            movieImdbId=imdbId,
            movieTitle="movieTitle",
            movieRuntimeInMinutes=90,
            movieReleaseDate=(TR.read "1999-03-31 00:00:00.000Z") :: UTCTime,
            movieFilmRating=FilmRating.R,
            movieGenre="Sci-Fi",
            movieDirector="Any",
            moviePlot="Some plot",
            movieMetascore=89,
            movieImdbRating=7.8,
            movieImdbVotes=12345
          }
