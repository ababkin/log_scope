module Types.API where

import           Haste.App (Client, Remote, Server)

data API = API {
    getRequestChunk     :: Remote (Server String)
  , performActionInVim  :: Remote (String -> Server ())
  }

data Action = OpenFile{
    filename :: String
  } deriving Show
