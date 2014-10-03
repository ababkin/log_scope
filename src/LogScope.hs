{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module LogScope where

import           Haste.App                 (addChild, liftIO, mkConfig, newElem,
                                            newTextElem, onServer, remote,
                                            runApp, runClient, setClass,
                                            withElem)
#ifndef __HASTE__

import           Control.Monad.Trans.Class (lift)
import           Pipes                     (Producer (..), runEffect, yield,
                                            (>->))
import           Pipes                     (await)
import qualified Pipes.Attoparsec          as PA
import           Pipes.Parse               (runStateT)
import qualified Pipes.Prelude             as P
import qualified Pipes.Text                as Text
import qualified Pipes.Text.IO             as Text
import           Data.Text


import           Parsers
import UdpServer (startUdpServer)

#endif

import           Control.Applicative       ((<$>))
import           Control.Monad             (join)
import           Haste                     (Event (..), onEvent)
import           Haste.App                 (MonadIO, alert)
import           Haste.DOM                 (Elem, setClass, toggleClass)
import           Haste.JSON
import           Haste.Prim
import           Haste.Serialize
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (MVar (..), newEmptyMVar, putMVar, takeMVar)

import           Types
import           Client (render)


#ifdef __HASTE__

requests = undefined
startUdpServer = undefined

#else

requestPipeParser :: (Monad m) => Producer Text m () -> Producer String m String
requestPipeParser s = do
  (r,s') <- lift (runStateT (PA.parse requestParser) s)
  case r of
    Nothing -> return ""
    Just (Left err) -> return ""
    Just (Right request) -> do
      let requestString = fromJSStr $ encodeJSON $ toJSON request
      (yield requestString) >> (return requestString)

requests :: MonadIO m => m String
requests = liftIO $ do
    requestJson <- runEffect $ do
      requestPipeParser Text.stdin >-> await
    return requestJson

#endif


main :: IO ()
main = do
  reqs <- newEmptyMVar
  forkIO $ startUdpServer reqs

  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    getRequest <- remote requests

    runClient $ withElem "requests" (render getRequest)

