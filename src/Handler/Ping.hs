{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Ping where

import Import

getAdminPingR :: Handler Value
getAdminPingR = return $ object [("message" .= ("pong" :: String))]
