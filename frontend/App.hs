{- import Haste.App -}
{- import Haste.Binary -}
{- import qualified Data.ByteString.Lazy as BS -}

{- main = withElems ["requests"] go -}

{- go [requests] = do -}
  {- request <- newElem "div" -}
  {- request `addChild` requests -}

import Haste.App
import qualified Haste.App.Concurrent as H

main :: IO ()
main = do
  runApp (mkConfig "ws://localhost:24601" 24601) $ do

    request <- remote $ do
      liftIO $ return "hello"

    runClient $ withElem "requests" $ \requests -> do
      req <- onServer $ request
      request <- newElem "div"
      requestText <- newTextElem req
      requestText `addChild` request
      request `addChild` requests
      {- setProp request "html" req -}

      {- req <- onServer $ request -}
      {- alert $ "The req is: " ++ req -}

