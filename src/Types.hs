{-# LANGUAGE OverloadedStrings #-}

module Types where

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
} deriving Show

instance Serialize Request where
  toJSON (Request verb path controller action) = Dict [
      ("verb",        toJSON verb)
    , ("path",        toJSON path)
    , ("controller",  toJSON controller)
    , ("action",      toJSON action)
    ]

  parseJSON j =
    Request <$>
        (j .: "verb")
    <*> (j .: "path")
    <*> (j .: "controller")
    <*> (j .: "action")

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
