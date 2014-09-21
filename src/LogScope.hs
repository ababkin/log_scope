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

import           Parsers

#endif

import           Control.Applicative       ((<$>))
import           Control.Monad             (join)
import           Data.Text
import           Haste                     (Event (..), onEvent)
import           Haste.App                 (MonadIO, alert)
import           Haste.DOM                 (Elem, setClass, toggleClass)
import           Haste.JSON
import           Haste.Prim
import           Haste.Serialize

import           Types


#ifdef __HASTE__

requests = undefined

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
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    getRequest <- remote requests

    runClient $ withElem "requests" (renderRequest getRequest)
      where
        renderRequest getRequest requestsContainer = do
          eitherRequest <- decodeJSON . toJSStr <$> onServer getRequest
          case join $ fromJSON <$> eitherRequest of
            Right req -> do
              addRequest req requestsContainer
              renderRequest getRequest requestsContainer
            Left err -> do
              addError err requestsContainer
              renderRequest getRequest requestsContainer

          where
            addRequest :: MonadIO m => Request -> Elem -> m ()
            addRequest req requestsContainer = do
              request <- newElem "div"

              appendTextElWithClasses "span" (verb req) ["label", verbCssClass req] request
              appendTextElWithClasses "div"  (path req) ["path"] request
              appendTextElWithClasses "div"  (controller req) ["controller"] request
              appendTextElWithClasses "div"  (action req) ["action"] request

              setClass request "request" True
              request `addChild` requestsContainer

              onEvent request OnClick $ \_ _ -> toggleRequestExpand request
              return ()


              where
                verbCssClass request = case verb req of
                  "GET"     -> "get"
                  "POST"    -> "post"
                  "PUT"     -> "put"
                  "DELETE"  -> "delete"
                  _         -> "unexpected_verb"

                toggleRequestExpand request = do
                  toggleClass request "expanded"

                appendTextElWithClasses :: MonadIO m => String -> String -> [String] -> Elem -> m ()
                appendTextElWithClasses tag text cssClasses parent = do
                  el <- newElem tag

                  textEl <- newTextElem text
                  textEl `addChild` el

                  mapM_ (\c -> setClass el c True) cssClasses
                  el `addChild` parent



            addError :: MonadIO m => String -> Elem -> m ()
            addError errorText requestsContainer = do
              err <- newElem "div"

              errText <- newTextElem errorText
              errText `addChild` err

              setClass err "alert" True
              setClass err "alert-danger" True
              err `addChild` requestsContainer


