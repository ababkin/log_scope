{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App                 (liftIO, mkConfig, remote,
                                            runApp, runClient,
                                            withElem)
#ifndef __HASTE__

import           Control.Monad             (forM_, forever)
import           Control.Monad.Trans.Class (lift)

import           Server.UdpServer          (startUdpServer)

#endif

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar (..), newEmptyMVar, putMVar,
                                            takeMVar)
import           Data.List.Split           (chunksOf)
import           Haste.App                 (MonadIO)
import           Haste.JSON                (encodeJSON)
import           Haste.Prim                (fromJSStr)
import           Haste.Serialize           (toJSON)

import           Client.Client             (render)
import           Types.Request             (Request)

#ifdef __HASTE__

requests = undefined
startUdpServer = undefined

#else


requests :: MonadIO m => MVar String -> m String
requests reqChunks = liftIO $ takeMVar reqChunks

maxStringLength = 4096

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

