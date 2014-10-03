module Client (render) where

import           Haste.App                 (addChild, newElem,
                                            newTextElem, onServer,
                                            setClass, setAttr,
                                            withElem, MonadIO, alert)
import           Control.Applicative       ((<$>))
import           Control.Monad             (join)
import           Haste.DOM                 (Elem, setClass, toggleClass)
import           Haste.JSON
import           Haste.Prim
import           Haste.Serialize

import Types

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
    addRequest :: MonadIO m => Request -> Elem -> m ()
    addRequest req requestsContainer = do
      request <- newElem "a"

      setAttr request "href" "#"
      setClass request "list-group-item" True

      input <- newElem "input"
      setAttr input "id" $ requestId n
      setAttr input "name" "accordion-1"
      setAttr input "type" "checkbox"
      input `addChild` request

      label <- newElem "label"
      setAttr label "for" $ requestId n
      
      requestShort <- newElem "span"
      appendTextElWithClasses "span" (verb req) ["label", verbCssClass req] requestShort
      appendTextElWithClasses "span"  (path req) ["path", "pull-right", "text-muted", "small"] requestShort
      {- appendTextElWithClasses "span"  (controller req) ["controller"] requestShort -}
      {- appendTextElWithClasses "span"  (action req) ["action"] requestShort -}

      setClass requestShort "request" True
      requestShort `addChild` label
      label `addChild` request

      article <- newElem "article"
      setClass article "ac-small" True
      textArticle <- newTextElem $ (controller req) ++ "#" ++ (action req)
      textArticle `addChild` article
      article `addChild` request



      request `addChild` requestsContainer

      {- onEvent request OnClick $ \_ _ -> toggleRequestExpand request -}
      return ()


      where
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

