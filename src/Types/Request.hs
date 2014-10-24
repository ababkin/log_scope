{-# LANGUAGE OverloadedStrings #-}

module Types.Request where

import           Control.Applicative ((<$>), (<*>))
import           Haste.JSON
import           Haste.Serialize

type Verb = String
type Path = String

data Request = Request {
    verb       :: Verb
  , path       :: Path
  , controller :: String
  , action     :: String
  , format     :: String
  , statusCode :: Int
  , timestamp  :: String
  , renderedPartials :: [PartialRendered]
  , sqlQueries :: [SqlQuery]
} deriving Show


instance Serialize Request where
  toJSON (Request verb path controller action format statusCode timestamp renderedPartials sqlQueries) = Dict [
      ("verb",        toJSON verb)
    , ("path",        toJSON path)
    , ("controller",  toJSON controller)
    , ("action",      toJSON action)
    , ("format",      toJSON format)
    , ("status",      toJSON statusCode)
    , ("timestamp",   toJSON timestamp)
    , ("renderedPartials", toJSON renderedPartials)
    , ("sqlQueries",  toJSON sqlQueries)
    ]

  parseJSON j =
    Request <$>
        (j .: "verb")
    <*> (j .: "path")
    <*> (j .: "controller")
    <*> (j .: "action")
    <*> (j .: "format")
    <*> (j .: "status")
    <*> (j .: "timestamp")
    <*> (j .: "renderedPartials")
    <*> (j .: "sqlQueries")




data PartialRendered = PartialRendered {
    prPath       :: String
  , prTimestamp  :: String
  , prDuration   :: Int
  } deriving Show

instance Serialize PartialRendered where
  toJSON (PartialRendered path timestamp duration) = Dict [
      ("path",        toJSON path)
    , ("timestamp",   toJSON timestamp)
    , ("duration",    toJSON duration)
    ]

  parseJSON j =
    PartialRendered <$>
        (j .: "path")
    <*> (j .: "timestamp")
    <*> (j .: "duration")


data SqlQuery = SqlQuery {
    sqSql        :: String
  , sqTimestamp  :: String
  , sqDuration   :: Int
  } deriving Show

instance Serialize SqlQuery where
  toJSON (SqlQuery sql timestamp duration) = Dict [
      ("sql",         toJSON sql)
    , ("timestamp",   toJSON timestamp)
    , ("duration",    toJSON duration)
    ]

  parseJSON j =
    SqlQuery <$>
        (j .: "sql")
    <*> (j .: "timestamp")
    <*> (j .: "duration")


{- instance FromJSON Request where -}
  {- parseJSON (Object v) = -}
    {- Request <$> -}
        {- (v .: "verb") -}
    {- <*> (v .: "path") -}
    {- <*> (v .: "controller") -}
    {- <*> (v .: "action") -}
  {- parseJSON o = typeMismatch "Request" o -}

{- instance ToJSON Request where -}
 {- toJSON (Request verb path controller action) = -}
    {- object [ "verb"       .= verb -}
           {- , "path"       .= path -}
           {- , "controller" .= controller -}
           {- , "action"     .= action -}
           {- ] -}
