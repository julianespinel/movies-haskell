{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MovieSpec (spec) where

import TestImport
import Data.Aeson
import TestFactories

spec :: Spec
spec = withApp $ do
    describe "get a single movie" $ do
        it "returns 200 and the movie we are looking for" $ do
          let imdbId = "imdbId" :: Text
              testMovie = getTestMovie imdbId
          _ <- runDB $ insert testMovie

          get("/movies/" ++ imdbId :: Text)
          statusIs 200
          bodyContains $ unpack imdbId

        it "returns 404 when movie does not exist" $ do
          get("/movies/123movie" :: Text)
          statusIs 404
          bodyContains "Movie does not exist"

    describe "update movie" $ do
        it "returns 200 and the updated movie" $ do
          let imdbId = "imdbId" :: Text
              testMovie = getTestMovie imdbId
          _ <- runDB $ insert testMovie

          let updatedTitle = "Matrix reloaded"
              updatedMovie = testMovie { movieTitle = updatedTitle }
              encodedMovie = encode updatedMovie

          request $ do
              setMethod "PUT"
              setUrl ("/movies/" ++ imdbId :: Text)
              setRequestBody encodedMovie
              addRequestHeader ("Content-Type", "application/json")

          statusIs 200
          bodyContains $ unpack imdbId
          bodyNotContains $ unpack (movieTitle testMovie)
          bodyContains $ unpack updatedTitle

        it "returns 404 whe movie does not exist" $ do
          let imdbId = "imdbId" :: Text
              testMovie = getTestMovie imdbId

          let updatedTitle = "Matrix reloaded"
              updatedMovie = testMovie { movieTitle = updatedTitle }
              encodedMovie = encode updatedMovie

          request $ do
              setMethod "PUT"
              setUrl ("/movies/123movie" :: Text)
              setRequestBody encodedMovie
              addRequestHeader ("Content-Type", "application/json")

          statusIs 404
          bodyContains "Movie does not exist"

    describe "delete movie" $ do
        it "returns 200 and deletes the movie if it exists" $ do
          let imdbId = "imdbId" :: Text
              testMovie = getTestMovie imdbId
          _ <- runDB $ insert testMovie

          performMethod "DELETE" ("/movies/" ++ imdbId :: Text)
          statusIs 200
          bodyContains "deleted"
          bodyContains "true"

          -- Check there are no movies in DB.
          moviesCount <- runDB $ count ([] :: [Filter Movie])
          assertEq "Should be zero " moviesCount 0

        it "returns 200 even if the movie does not exist" $ do
          performMethod "DELETE" ("/movies/123movie" :: Text)
          statusIs 200
          bodyContains "deleted"
          bodyContains "true"
