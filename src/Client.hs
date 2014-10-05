module Client (render) where

import           Control.Applicative ((<$>))
import           Control.Monad       (join)
import           Haste.App           (MonadIO, addChild, alert, liftIO, newElem,
                                      newTextElem, onServer, setAttr, setClass,
                                      withElem)
import           Haste.DOM           (Elem, setClass, toggleClass)
import           Haste.JSON          (decodeJSON)
import           Haste.Perch         (atr, build, div, p, (!))
import           Haste.Prim
import           Haste.Serialize
import           Prelude             hiding (div, (!))

import           Types

render = renderRequest 0

renderRequest n getRequest requestsContainer = do
  eitherRequest <- decodeJSON . toJSStr <$> onServer getRequest
  case join $ fromJSON <$> eitherRequest of
    Right req -> do
      addRequest req requestsContainer
      renderRequest (n + 1) getRequest requestsContainer
    Left err -> do
      addError err requestsContainer
      renderRequest (n + 1) getRequest requestsContainer

  where
    {- addRequest :: MonadIO m => Request -> Elem -> m () -}
    {- addRequest :: Request -> Elem -> Client () -}
    addRequest req requestsContainer = do
      request <- newElem "a"

      setAttr request "href" "#"
      setClass request "list-group-item" True

      input <- newElem "input"
      setAttr input "id" $ requestId n
      setAttr input "name" "accordion-1"
      setAttr input "type" "checkbox"
      addChild input request

      label <- newElem "label"
      setAttr label "for" $ requestId n

      requestBrief <- newElem "span"
      appendTextElWithClasses "span" (verb req) ["label", verbCssClass req] requestBrief
      appendTextElWithClasses "span"  (path req) ["path", "text-muted", "small"] requestBrief
      appendTextElWithClasses "span"  (path req) ["timestamp", "pull-right", "text-muted", "small"] requestBrief
      {- appendTextElWithClasses "span"  (controller req) ["controller"] requestShort -}
      {- appendTextElWithClasses "span"  (action req) ["action"] requestShort -}

      setClass requestBrief "request" True
      addChild requestBrief label
      addChild label request

      article <- newElem "article"
      setClass article "ac-small" True
      requestDetails req article
      addChild article request
      addChild request requestsContainer

      {- onEvent request OnClick $ \_ _ -> toggleRequestExpand request -}
      return ()


      where
        {- requestDetails :: MonadIO m => Request -> Elem -> m Elem -}
        {- requestDetails :: Request -> Elem -> Client Elem -}
        requestDetails req parent = do
          {- table <- newElem "table" -}
          {- addChild () -}
          {- newTextElem $ (controller req) ++ "#" ++ (action req) -}
          {- addChild table parent -}


{- withElem "blah" :: MonadIO m => (Elem -> m a) -> m a -}


{- build :: Elem -> IO Elem -}

         liftIO $ build detailsPerch parent
            where
              detailsPerch = do
                div $ do
                  {- addEvent this OnClick $ \_ _ -> alert "hello, world!" -}
                  div $ do
                    p "hello"
                    p ! atr "style" "color:red" $ "world"


        requestId :: Int -> String
        requestId n = "ac-" ++ (show n)

        verbCssClass request = case verb req of
          "GET"     -> "get"
          "POST"    -> "post"
          "PUT"     -> "put"
          "DELETE"  -> "delete"
          _         -> "unexpected_verb"

        {- toggleRequestExpand request = do -}
          {- toggleClass request "expanded" -}

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

