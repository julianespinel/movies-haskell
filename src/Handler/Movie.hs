{-# LANGUAGE OverloadedStrings #-}

module Handler.Movie where

-- import qualified Data.HashMap.Strict as Map
import Import

getMovieR :: MovieId -> Handler Value
getMovieR movieId = do
  movie <- runDB $ get404 movieId
  return $ object ["movie" .= (Entity movieId movie)]

putMovieR :: MovieId -> Handler Value
putMovieR movieId = do
  movie <- requireJsonBody :: Handler Movie
  runDB $ replace movieId movie
  return $ object ["movie" .= (Entity movieId movie)]

deleteMovieR :: MovieId -> Handler Value
deleteMovieR movieId = do
  runDB $ delete movieId
  -- let response = Map.fromList [("deleted", True)]
  return $ object [("deleted" .= (True))]
  -- return $ object [("msg" .= ("Hello world" :: String))]
  -- returnJson response
