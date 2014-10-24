module Client.UI.Request (addRequest) where

import           Haste.App       (Client, MonadIO, liftIO)
import           Haste.DOM       (Elem)
import           Haste.Perch     (Perch, a, atr, build, div, h4, li, p, span,
                                  table, tbody, td, tr, ul, (!))
import           Haste.Prim
import           Haste.Serialize
import           Prelude         hiding (div, span, (!))

import           Types.Request


{- addRequest :: MonadIO m => Request -> Elem -> m () -}
addRequest :: Request -> Int -> Elem -> Client ()
addRequest req n requestsContainer = liftIO $ build (addRequestPerch req n) requestsContainer >> return ()

addRequestPerch :: Request -> Int -> Perch
addRequestPerch req n = do
  div ! atr "class" "panel panel-default" $ do
    div ! atr "class" "panel-heading" $ do
      h4 ! atr "class" "panel-title clearfix" $ do
        div ! atr "data-toggle" "collapse" ! atr "href" (genHref "collapse" n) $ do
          div ! atr "class" "request" $ do
            span ! atr "class" (addVerbClass req "label") $ verb req
            span ! atr "class" "path text-muted small" $ "link"
            span ! atr "class" "timestamp pull-right text-muted small" $ "1:30:29"
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
          tr $ do
            td "joan_rivers/style_guide/components/_role_switcher.html.haml"
            td "1.5ms"

    sqlQueriesTable req = do
      table ! atr "class" "table sql" $ do
        tbody $ do
          tr $ do
            td "SELECT 1 AS one FROM \"users\" INNER JOIN \"investor_group_memberships\" ON \"users\".\"id\" = \"investor_group_memberships\".\"person_id\" WHERE \"investor_group_memberships\".\"investor_group_id\" = 23 AND \"investor_group_memberships\".\"admin\" = 't' AND \"users\".\"id\" = 53 LIMIT 1"
            td "3.1ms"


    addVerbClass req classes =  classes ++ " " ++ (verbCssClass $ verb req)

    verbCssClass request = case verb req of
      "GET"     -> "get"
      "POST"    -> "post"
      "PUT"     -> "put"
      "DELETE"  -> "delete"
      _         -> "unexpected_verb"




