{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App                 (addChild, liftIO, mkConfig, newElem,
                                            newTextElem, onServer, remote,
                                            runApp, runClient, setClass,
                                            withElem)
#ifndef __HASTE__

import           Control.Monad.Trans.Class (lift)
import           Data.Text
import           Pipes                     (Producer (..), runEffect, yield,
                                            (>->))
import           Pipes                     (await)
import qualified Pipes.Attoparsec          as PA
import           Pipes.Parse               (runStateT)
import qualified Pipes.Prelude             as P
import qualified Pipes.Text                as Text
import qualified Pipes.Text.IO             as Text


import           Parsers
import           Server.UdpServer          (startUdpServer)

#endif

import           Control.Applicative       ((<$>))
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar (..), newEmptyMVar, putMVar,
                                            takeMVar)
import           Control.Monad             (join)
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

{- requestPipeParser :: (Monad m) => Producer Text m () -> Producer String m String -}
{- requestPipeParser s = do -}
  {- (r,s') <- lift (runStateT (PA.parse requestParser) s) -}
  {- case r of -}
    {- Nothing -> return "" -}
    {- Just (Left err) -> return "" -}
    {- Just (Right request) -> do -}
      {- let requestString = fromJSStr $ encodeJSON $ toJSON request -}
      {- (yield requestString) >> (return requestString) -}

{- requests :: MonadIO m => m String -}
{- requests = liftIO $ do -}
    {- requestJson <- runEffect $ do -}
      {- requestPipeParser Text.stdin >-> await -}
    {- return requestJson -}

requests :: MonadIO m => MVar Request -> m String
requests reqs = liftIO $ (fromJSStr . encodeJSON . toJSON) <$> takeMVar reqs

#endif


main :: IO ()
main = do
  reqs <- newEmptyMVar
#ifndef __HASTE__
  forkIO $ startUdpServer reqs
#endif

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    getRequest <- remote $ requests reqs
    runClient $ withElem "requests" (render getRequest)

