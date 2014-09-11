{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App                 (addChild, liftIO, mkConfig, newElem,
                                            newTextElem, onServer, remote,
                                            runApp, runClient, withElem)

{- import           Data.Text -}
{- import           Control.Monad.Trans.Class (lift) -}
{- import           Pipes                     (Producer (..), runEffect, yield, -}
                                            {- (>->)) -}
{- import           Pipes                     (await) -}
{- import qualified Pipes.Attoparsec          as PA -}
{- import           Pipes.Parse               (runStateT) -}
{- import qualified Pipes.Prelude             as P -}
{- import qualified Pipes.Text                as Text -}
{- import qualified Pipes.Text.IO             as Text -}

{- import           Parsers -}
{- import           Types -}


{- requests :: (Monad m) => Producer Text m () -> Producer Request m String -}
{- requests s = do -}
  {- (r,s') <- lift (runStateT (PA.parse requestParser) s) -}
  {- case r of -}
    {- Nothing -> return "" -}
    {- Just (Left err) -> return "" -}
    {- Just (Right request) -> yield request >> (return "") -}

{- main :: IO () -}
{- main = do -}
  {- runEffect $ do -}
    {- requests Text.stdin >-> P.map show >-> P.stdoutLn -}


main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    getRequest <- remote $ do
      liftIO getLine
      {- liftIO $ runEffect $ do -}
        {- requests Text.stdin >-> P.map show >-> await -}

    runClient $ withElem "requests" (renderRequest getRequest)
      where
        renderRequest getRequest requests = do
          req <- onServer getRequest
          request <- newElem "div"
          requestText <- newTextElem req
          requestText `addChild` request
          request `addChild` requests
          renderRequest getRequest requests


