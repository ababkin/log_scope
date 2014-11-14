{-# LANGUAGE ForeignFunctionInterface #-}

module Client.Client (render, addRequest) where

import           Control.Applicative ((<$>))
import           Control.Monad       ((<=<))
import           Control.Monad.Loops (unfoldWhileM)
import           Data.Monoid         (mconcat)
import           Haste               hiding (click)
import           Haste.App           (Client, MonadIO, Remote, Server, addChild,
                                      alert, liftIO, newElem, newTextElem,
                                      onServer)
import           Haste.DOM           (Elem, setClass)
import           Haste.JSON          (decodeJSON)
import           Haste.Prim          (toJSStr)
import           Haste.Serialize     (Serialize, fromJSON)

import           Client.UI.Request   (addRequest)
import           Types.Request       (Request)
import Types.API (API(..), Action)



foreign import ccall scrollDown :: IO ()

render = renderRequest 0

renderRequest :: Int -> API -> Elem -> Client ()
renderRequest n api container = do
  getPayload (getRequestChunk api) >>= either (addError container) (addRequest api container n)

  liftIO $ scrollDown
  renderRequest (n + 1) api container

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



