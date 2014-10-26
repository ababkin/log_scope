{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App                 (addChild, liftIO, mkConfig, newElem,
                                            newTextElem, onServer, remote,
                                            runApp, runClient, setClass,
                                            withElem)
#ifndef __HASTE__

import           Control.Monad             (forM_, forever)
import           Control.Monad.Trans.Class (lift)
import           Pipes                     (Producer (..), runEffect, yield,
                                            (>->))
import           Pipes                     (await)
import qualified Pipes.Attoparsec          as PA
import           Pipes.Parse               (runStateT)
import qualified Pipes.Prelude             as P
import qualified Pipes.Text                as Text
import qualified Pipes.Text.IO             as Text

import           Server.UdpServer          (startUdpServer)

#endif

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar (..), newEmptyMVar, putMVar,
                                            takeMVar)
import           Control.Monad             (join)
import           Data.List.Split           (chunksOf)
import           Haste                     (Event (..), onEvent)
import           Haste.App                 (MonadIO, alert)
import           Haste.DOM                 (Elem, setClass, toggleClass)
import           Haste.JSON
import           Haste.Prim
import           Haste.Serialize

import           Client.Client             (render)
import           Types.Request

#ifdef __HASTE__

requests = undefined
startUdpServer = undefined

#else


requests :: MonadIO m => MVar String -> m String
requests reqChunks = liftIO $ takeMVar reqChunks

maxStringLength = 1024

requestPump :: MVar Request -> MVar String -> IO ()
requestPump reqs reqChunks = forever $ do
  reqS <- (fromJSStr . encodeJSON . toJSON) <$> takeMVar reqs
  forM_ (chunksOf maxStringLength reqS) $ putMVar reqChunks
  putMVar reqChunks ""

#endif


main :: IO ()
main = do
  reqs      <- newEmptyMVar
  reqChunks <- newEmptyMVar
#ifndef __HASTE__
  forkIO $ startUdpServer reqs
  forkIO $ requestPump reqs reqChunks
#endif

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    getRequestChunk <- remote $ requests reqChunks

    runClient $ withElem "requests" (render getRequestChunk)

