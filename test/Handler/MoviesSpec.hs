{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MoviesSpec (spec) where

import TestImport
import Data.Aeson
import FilmRating
import qualified Text.Read as TR

spec :: Spec
spec = withApp $ do
    describe "creates a movie" $ do
        it "returns 201 and movie is created in DB" $ do
            let imdbId = "imdbId" :: Text
                title = "movieTitle" :: Text
                runtimeInMinutes = 1 :: Int
                releaseDate = "1999-03-31 00:00:00.000Z" :: Text
                filmRating = "R" :: Text
                genre = "Sci-Fi" :: Text
                director = "The Wachowski brothers" :: Text
                plot = "plot" :: Text
                metascore = 90 :: Int
                imdbRating = 8.3 :: Double
                imdbVotes = 123 :: Int

                body = object [ "imdbId" .= imdbId,
                                "title" .= title,
                                "runtimeInMinutes" .= runtimeInMinutes,
                                "releaseDate" .= releaseDate,
                                "filmRating" .= filmRating,
                                "genre" .= genre,
                                "director" .= director,
                                "plot" .= plot,
                                "metascore" .= metascore,
                                "imdbRating" .= imdbRating,
                                "imdbVotes" .= imdbVotes
                              ]
                encodedMovie = encode body

            request $ do
                setMethod "POST"
                setUrl MoviesR
                setRequestBody encodedMovie
                addRequestHeader ("Content-Type", "application/json")

            -- Check http response.
            statusIs 201
            bodyContains $ unpack imdbId

            -- Check movie was saved in DB.
            [Entity _id movie] <- runDB $ selectList [MovieImdbId ==. imdbId] []
            -- movieImdbId :: Movie -> Text (Yesod generated function)
            assertEq "Should have " (movieImdbId movie) imdbId

        it "returns 400 when JSON body is invalid" $ do
            post MoviesR

            let body = object [ "foo" .= ("My message" :: Value) ]

            request $ do
                setMethod "POST"
                setUrl MoviesR
                setRequestBody $ encode body
                addRequestHeader ("Content-Type", "application/json")

            statusIs 400

    describe "get movies" $ do
        it "returns 200 and an empty list" $ do
            let imdbId = "imdbId" :: Text
            get MoviesR
            statusIs 200
            bodyNotContains $ unpack imdbId
            -- Check there are no movies in DB.
            moviesCount <- runDB $ count ([] :: [Filter Movie])
            assertEq "Should be zero " moviesCount 0

        it "returns 200 and a non-empty list" $ do
          let imdbId = "imdbId" :: Text
          let movie = Movie {
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
          _ <- runDB $ insert movie

          get MoviesR
          statusIs 200
          bodyContains $ unpack imdbId
          -- Check there is one movies in DB.
          moviesCount <- runDB $ count ([] :: [Filter Movie])
          assertEq "Should be zero " moviesCount 1
