module Client.UI.Request (addRequest) where

import           Control.Monad   (forM)
import           Data.Foldable   (foldMap)
import           Haste.App       (Client, MonadIO, liftIO)
import           Haste.DOM       (Elem)
import           Haste.Perch     (Perch, a, atr, build, div, h4, li, p, span,
                                  table, tbody, td, tr, ul, (!))
import           Haste.Prim
import           Haste.Serialize
import           Prelude         hiding (div, span, (!))

import           Types.Request


addRequest :: Elem -> Int -> Request -> Client ()
addRequest requestsContainer n req = liftIO $ build (addRequestPerch req n) requestsContainer >> return ()

addRequestPerch :: Request -> Int -> Perch
addRequestPerch req n = do
  div ! atr "class" "panel panel-default" $ do
    div ! atr "class" "panel-heading" $ do
      h4 ! atr "class" "panel-title clearfix" $ do
        div ! atr "data-toggle" "collapse" ! atr "href" (genHref "collapse" n) $ do
          div ! atr "class" "request" $ do
            span ! atr "class" (addVerbClass req "label") $ verb req
            span ! atr "class" "path text-muted small" $ path req
            span ! atr "class" "timestamp pull-right text-muted small" $ timestamp req
    div ! atr "id" (genId "collapse" n) ! atr "class" "panel-collapse collapse" $ do
      div ! atr "class" "panel-body" $ do
        ul ! atr "id" "tab-test" ! atr "class" "nav nav-pills" $ do
          li ! atr "class" "active" ! atr "data-toggle" "pill" $ do
            a ! atr "href" (genHref "general_info" n)       $ "General Info"
          li ! atr "data-toggle" "pill" $ do
            a ! atr "href" (genHref "rendered_templates" n) $ "Rendered Templates"
          li ! atr "data-toggle" "pill" $ do
            a ! atr "href" (genHref "sql_queries" n)                $ "SQL"


        div ! atr "class" "tab-content" $ do
          div ! atr "class" "tab-pane active" ! atr "id" (genId "general_info" n)       $ generalInfoTable req
          div ! atr "class" "tab-pane"        ! atr "id" (genId "rendered_templates" n) $ renderedTemplatesTable req
          div ! atr "class" "tab-pane"        ! atr "id" (genId "sql_queries" n)        $ sqlQueriesTable req

  where
    genHref s n = "#" ++ genId s n
    genId s n = s ++ "_" ++ show n

    generalInfoTable req = do
      table ! atr "class" "table general" $ do
        tbody $ do
          tr $ do
            td "Method"
            td $ verb req
          tr $ do
            td "Controller"
            td $ controller req
          tr $ do
            td "Action"
            td $ action req
          tr $ do
            td "Path"
            td $ path req
          tr $ do
            td "Status"
            td $ statusCode req

    renderedTemplatesTable req = do
      table ! atr "class" "table templates" $ do
        tbody $ do
          foldMap renderPartial $ renderedPartials req
      where
        renderPartial partial = do
          tr $ do
            td $ prPath partial
            td $ prTimestamp partial

    sqlQueriesTable req = do
      table ! atr "class" "table sql" $ do
        tbody $ do
          foldMap renderSqlQuery $ sqlQueries req
      where
        renderSqlQuery query = do
          tr $ do
            td ! atr "class" "query" $ sqSql query
            td $ sqTimestamp query


    addVerbClass req classes =  classes ++ " " ++ (verbCssClass $ verb req)

    verbCssClass request = case verb req of
      "GET"     -> "get"
      "POST"    -> "post"
      "PUT"     -> "put"
      "DELETE"  -> "delete"
      _         -> "unexpected_verb"




