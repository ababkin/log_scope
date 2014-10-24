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

import Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 as LBS
import Codec.Compression.Zlib (decompress)

foreign import ccall scrollDown :: IO ()


lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS = BS.concat . LBS.toChunks

strictToLazyBS :: BS.ByteString -> LBS.ByteString
strictToLazyBS = LBS.fromChunks . (splitIntoChunks 1024 []) -- . BS.concat


splitIntoChunks n ss s  | BS.length s <= n  = (BS.take n s):ss
                        | otherwise         = (BS.take n s):(splitIntoChunks n ss $ BS.drop n s)


render = renderRequest 0

renderRequest :: Int -> Remote (Server String) -> Elem -> Client ()
renderRequest n getRequest requestsContainer = do
  eitherRequest <- decodeJSON . toJSStr . BS.unpack . lazyToStrictBS . decompress . strictToLazyBS . BS.pack <$> onServer getRequest
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



