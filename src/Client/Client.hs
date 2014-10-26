{-# LANGUAGE ForeignFunctionInterface #-}

module Client.Client (render, addRequest) where

import           Control.Applicative ((<$>))
import           Control.Monad       ((<=<))
import           Control.Monad.Loops (unfoldWhileM)
import           Data.Monoid         (mconcat)
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
renderRequest n getRequestChunk container = do
  getPayload getRequestChunk >>= either (addError container) (addRequest container n)

  liftIO $ scrollDown
  renderRequest (n + 1) getRequestChunk container

  where
    getPayload :: Serialize a => Remote (Server String) -> Client (Either String a)
    getPayload getChunk = (fromJSON <=< decodeJSON . toJSStr . mconcat) <$> (unfoldWhileM (/="") $ onServer getChunk)


addError :: MonadIO m => Elem -> String -> m ()
addError requestsContainer errorText = do
  err <- newElem "div"

  errText <- newTextElem errorText
  errText `addChild` err

  setClass err "alert" True
  setClass err "alert-danger" True
  err `addChild` requestsContainer



