{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Movies where

import Import
import Database.Persist.Sql (rawSql)

postMoviesR :: Handler Value
postMoviesR = do
    -- requireJsonBody will parse the request body into the appropriate type,
    -- or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    movie <- requireJsonBody :: Handler Movie
    insertedMovie <- runDB $ insertEntity movie
    sendStatusJSON created201 (object ["movie" .= insertedMovie])

getMoviesR :: Handler Value
getMoviesR = do
  params <- reqGetParams <$> getRequest
  movies <- getMovies params
  returnJson $ object ["movies" .= movies]

getMovies :: [a] -> Handler [Entity Movie]
getMovies [] = runDB $ selectList [] []
getMovies _ = getMoviesFilter

wrap :: Text -> Maybe Text -> Maybe Text
wrap wrapper (Just word) = Just(wrapper ++ word ++ wrapper)
wrap wrapper Nothing     = Just(wrapper)

getMoviesFilter :: Handler [Entity Movie]
getMoviesFilter = do
  title <- lookupGetParam "title"
  runtimeInMinutes <- lookupGetParam "runtimeInMinutes"
  metascore <- lookupGetParam "metascore"
  imdbRating <- lookupGetParam "imdbRating"
  imdbVotes <- lookupGetParam "imdbVotes"
  let values = [(wrap "%" title), runtimeInMinutes, metascore, imdbRating, imdbVotes]
  runDB $ rawSql sql $ map toPersistValue values
    where sql = "SELECT ?? FROM movie WHERE title LIKE ? AND \
                \runtime_in_minutes >= ? AND metascore >= ? AND \
                \imdb_rating >= ? AND imdb_votes >= ?"
