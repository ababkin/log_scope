{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Applicative ((<$>), (<*>))
import           Data.Text
import           Haste.JSON
import           Haste.Serialize

type Verb = Text
type Path = Text

data Request = Request {
    verb       :: Verb
  , path       :: Path
  , controller :: Text
  , action     :: Text
} deriving Show


instance Serialize Request where
  toJSON (Request verb path controller action) = Dict [
      ("verb", toJSON $ unpack verb)
    , ("path", toJSON $ unpack path)
    , ("controller", toJSON $ unpack controller)
    , ("action", toJSON $ unpack action)
    ]

  parseJSON j =
    Request <$>
        (pack <$> (j .: "path"))
    <*> (pack <$> (j .: "verb"))
    <*> (pack <$> (j .: "controller"))
    <*> (pack <$> (j .: "action"))

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
