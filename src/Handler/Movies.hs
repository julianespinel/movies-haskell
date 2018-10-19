module Handler.Movies where

import Import

postMoviesR :: Handler Value
postMoviesR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    movie <- requireJsonBody :: Handler Movie
    insertedMovie <- runDB $ insertEntity movie
    returnJson insertedMovie

getMoviesR :: Handler Value
getMoviesR = do
  movies <- runDB $ selectList [] [] :: Handler [Entity Movie]
  returnJson movies
