{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.PingSpec (spec) where

import TestImport
import Data.Aeson
import TestFactories

spec :: Spec
spec = withApp $ do
    describe "check if program is alive" $ do
        it "returns 200 and a ping message" $ do
          get("/admin/ping" :: Text)

          -- Check http response.
          statusIs 200
          bodyContains "{\"message\":\"pong\"}"