{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App     (addChild, liftIO, mkConfig, newElem,
                                newTextElem, onServer, remote, runApp,
                                runClient, setClass, withElem)

import           Client.Client (addRequest)
import           Types.Request (Request (..))

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    runClient $ withElem "requests" $ \requestsContainer -> do
      let req = Request{
          verb = "Get"
        , path = "/organizations/when-i-group-you-group/folders/538"
        , controller = "ApplicationController"
        , action = "index"
        , statusCode = 200
        }

      addRequest requestsContainer 1 req

      {- article <- newElem "article" -}
      {- addChild article requestsContainer -}


