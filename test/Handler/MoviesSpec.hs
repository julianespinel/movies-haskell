{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MoviesSpec (spec) where

import TestImport
import Data.Aeson
import TestFactories

spec :: Spec
spec = withApp $ do
    describe "creates a movie" $ do
        it "returns 201 and movie is created in DB" $ do
            let imdbId = "imdbId" :: Text
                testMovie = getTestMovie imdbId
                encodedMovie = encode testMovie

            request $ do
                setMethod "POST"
                setUrl MoviesR
                setRequestBody encodedMovie
                addRequestHeader ("Content-Type", "application/json")

            -- Check http response.
            statusIs 201
            bodyContains "movie"
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
            bodyContains "movies"
            bodyNotContains $ unpack imdbId
            -- Check there are no movies in DB.
            moviesCount <- runDB $ count ([] :: [Filter Movie])
            assertEq "Should be zero " moviesCount 0

        it "returns 200 and a non-empty list" $ do
          let imdbId = "imdbId" :: Text
              testMovie = getTestMovie imdbId
          _ <- runDB $ insert testMovie

          get MoviesR
          statusIs 200
          bodyContains "movies"
          bodyContains $ unpack imdbId
          -- Check there is one movies in DB.
          moviesCount <- runDB $ count ([] :: [Filter Movie])
          assertEq "Should be zero " moviesCount 1


    describe "filter movies" $ do
        it "returns 200 and an empty list" $ do
          let imdbId = "imdbId" :: Text
          get("/movies?title=mov&runtimeInMinutes=80&metascore=79&imdbRating=7.2&imdbVotes=1000" :: Text)
          statusIs 200
          bodyContains "movies"
          bodyNotContains $ unpack imdbId
          -- Check there are no movies in DB.
          moviesCount <- runDB $ count ([] :: [Filter Movie])
          assertEq "Should be zero " moviesCount 0

        it "returns 200 and a non-empty list" $ do
          let imdbId = "imdbId" :: Text
              testMovie = getTestMovie imdbId
          _ <- runDB $ insert testMovie

          get("/movies?title=mov&runtimeInMinutes=80&metascore=79&imdbRating=7.2&imdbVotes=1000" :: Text)
          statusIs 200
          bodyContains "movies"
          bodyContains $ unpack imdbId
          -- Check there is one movies in DB.
          moviesCount <- runDB $ count ([] :: [Filter Movie])
          assertEq "Should be zero " moviesCount 1
