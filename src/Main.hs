{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>), many)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Data.Attoparsec.Text (Parser(..), char, string, takeTill, skipWhile, endOfLine, isEndOfLine)
import           Data.Text

import           Pipes (Producer(..), runEffect, yield, (>->))
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import           Pipes.Parse (runStateT)
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text


type Verb = Text
type Path = Text

data Request = Request {
    verb        :: Verb
  , path        :: Path
  , controller  :: Text
  , action      :: Text
} deriving Show

requestParser :: Parser Request
requestParser =
    requestParser'
  <|>
    skipWhile (not . isEndOfLine ) *> endOfLine *> requestParser

requestParser' :: Parser Request
requestParser' = do
  verb <- requestLineParser
  (controller, action) <- processingByLineParser
  return $ Request verb "some/path" controller action


requestLineParser :: Parser Verb
requestLineParser =
  (string "Started ") *> (takeTill (==' ')) <* skipWhile (not . isEndOfLine ) <* endOfLine

{- requestLineParser :: Parser Text -}
{- requestLineParser = -}
  {- string "Started " -}
  {- verb <- takeTill (==' ') -}
  {- skipWhile (not . isEndOfLine ) -}
  {- endOfLine -}
  {- return verb -}


{- processingByLineParser :: Parser (Text, Text) -}
{- processingByLineParser = -}
  {- (,) <$> ((string "Processing by ") *> (takeTill (=='#')) <*> (takeTill (==' ')) <* skipWhile (not . isEndOfLine ) <* endOfLine) -}

processingByLineParser :: Parser (Text, Text)
processingByLineParser = do
  string "Processing by "
  controller <- takeTill (=='#')
  char '#'
  action <- takeTill (==' ')
  skipWhile (not . isEndOfLine)
  endOfLine
  return (controller, action)




requests :: (Monad m) => Producer Text m () -> Producer Request m ()
requests s = do
  (r,s') <- lift (runStateT (PA.parse requestParser) s)
  case r of
    Nothing -> return ()
    Just (Left err) -> return ()
    Just (Right request) -> yield request >> requests s'


main :: IO ()
main = do
  runEffect $ do
    requests Text.stdin >-> P.map show >-> P.stdoutLn
