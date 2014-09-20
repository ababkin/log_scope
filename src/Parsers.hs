{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import           Control.Applicative  (many, (*>), (<$>), (<*), (<*>), (<|>))
import           Control.Monad
import           Data.Attoparsec.Text (Parser (..), char, endOfLine,
                                       isEndOfLine, skipWhile, string, takeTill)
import           Data.Text

import           Types


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
