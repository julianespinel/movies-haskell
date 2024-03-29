{-# LANGUAGE OverloadedStrings #-}

module Handler.Movie where

import Import

movieNotFoundError :: Value
movieNotFoundError = object ["error" .= ("Movie does not exist" :: String)]

getMovieR :: MovieId -> Handler Value
getMovieR movieId = do
  maybeMovie <- runDB $ get movieId
  case maybeMovie of
    Just(movie) -> return $ object ["movie" .= (Entity movieId movie)]
    Nothing -> sendStatusJSON notFound404 movieNotFoundError

putMovieR :: MovieId -> Handler Value
putMovieR movieId = do
  maybeMovie <- runDB $ get movieId
  case maybeMovie of
    Just(_) -> do
      movie <- requireJsonBody :: Handler Movie
      runDB $ replace movieId movie
      return $ object ["movie" .= (Entity movieId movie)]
    Nothing -> do
      sendStatusJSON notFound404 movieNotFoundError

deleteMovieR :: MovieId -> Handler Value
deleteMovieR movieId = do
  runDB $ delete movieId
  return $ object [("deleted" .= (True))]
