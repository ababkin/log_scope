{- import Haste.App -}
{- import Haste.Binary -}
{- import qualified Data.ByteString.Lazy as BS -}

{- main = withElems ["requests"] go -}

{- go [requests] = do -}
  {- request <- newElem "div" -}
  {- request `addChild` requests -}

import Haste.App
import System.IO (getLine)

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    getRequest <- remote $ do
      liftIO getLine

    runClient $ withElem "requests" (renderRequest getRequest)
      where
        renderRequest getRequest requests = do
          req <- onServer getRequest
          request <- newElem "div"
          requestText <- newTextElem req
          requestText `addChild` request
          request `addChild` requests
          renderRequest getRequest requests




          {- setProp request "html" req -}

          {- req <- onServer $ request -}
          {- alert $ "The req is: " ++ req -}

