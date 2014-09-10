module Types where

import           Data.Text

type Verb = Text
type Path = Text

data Request = Request {
    verb       :: Verb
  , path       :: Path
  , controller :: Text
  , action     :: Text
} deriving Show
