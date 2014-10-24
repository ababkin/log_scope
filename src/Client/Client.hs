{-# LANGUAGE ForeignFunctionInterface #-}

module Client.Client (render, addRequest) where

import           Control.Applicative ((<$>))
import           Control.Monad       (join)
import           Haste               hiding (click)
import           Haste.App           (Client, MonadIO, Remote, Server, addChild,
                                      alert, liftIO, newElem, newTextElem,
                                      onServer, setAttr, setClass, withElem)
import           Haste.DOM           (Elem, setClass, toggleClass)
import           Haste.JSON          (decodeJSON)
import           Haste.Prim
import           Haste.Serialize


import           Client.UI.Request
import           Types.Request


foreign import ccall scrollDown :: IO ()

render = renderRequest 0

renderRequest :: Int -> Remote (Server String) -> Elem -> Client ()
renderRequest n getRequest requestsContainer = do
  eitherRequest <- decodeJSON . toJSStr <$> onServer getRequest
  case join $ fromJSON <$> eitherRequest of
    Right req -> do
      addRequest req n requestsContainer
      liftIO $ scrollDown

      renderRequest (n + 1) getRequest requestsContainer
    Left err -> do
      addError err requestsContainer
      liftIO $ scrollDown

      renderRequest (n + 1) getRequest requestsContainer

addError :: MonadIO m => String -> Elem -> m ()
addError errorText requestsContainer = do
  err <- newElem "div"

  errText <- newTextElem errorText
  errText `addChild` err

  setClass err "alert" True
  setClass err "alert-danger" True
  err `addChild` requestsContainer
