{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MovieSpec (spec) where

import TestImport
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
