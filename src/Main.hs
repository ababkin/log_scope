{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import           Haste.App                 (addChild, liftIO, mkConfig, newElem, setClass,
                                            newTextElem, onServer, remote,
                                            runApp, runClient, withElem)
#ifndef __HASTE__

import           Data.Text
import           Control.Monad.Trans.Class (lift)
import           Pipes                     (Producer (..), runEffect, yield,
                                            (>->))
import           Pipes                     (await)
import qualified Pipes.Attoparsec          as PA
import           Pipes.Parse               (runStateT)
import qualified Pipes.Prelude             as P
import qualified Pipes.Text                as Text
import qualified Pipes.Text.IO             as Text

import           Parsers
import           Types

#endif



#ifdef __HASTE__

requests = undefined

#else

requestPipeParser :: (Monad m) => Producer Text m () -> Producer Request m String
requestPipeParser s = do
  (r,s') <- lift (runStateT (PA.parse requestParser) s)
  case r of
    Nothing -> return ""
    Just (Left err) -> return ""
    Just (Right request) -> yield request >> (return "")

requests = do
  liftIO $ runEffect $ do
    requestPipeParser Text.stdin >-> P.map show >-> await

#endif


main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    getRequest <- remote requests

    runClient $ withElem "requests" (renderRequest getRequest)
      where
        renderRequest getRequest requestsContainer = do
          req <- onServer getRequest
          request <- newElem "div"
          requestText <- newTextElem req
          requestText `addChild` request
          setClass request "panel" True
          request `addChild` requestsContainer
          renderRequest getRequest requestsContainer


