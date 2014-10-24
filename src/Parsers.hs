{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import           Control.Applicative  (many, (*>), (<$>), (<*), (<*>), (<|>))
import           Control.Monad
import           Data.Attoparsec.Text (Parser (..), char, char, endOfLine,
                                       isEndOfLine, skipWhile, space, string,
                                       takeTill)
import           Data.Text            (Text, unpack)

import           Types.Request        (Request (..))


requestParser :: Parser Request
requestParser =
    requestParser'
  <|>
    skipWhile (not . isEndOfLine ) *> endOfLine *> requestParser

requestParser' :: Parser Request
requestParser' = do
  (verb, path) <- requestLineParser
  (controller, action) <- processingByLineParser
  return $ Request
    (unpack verb)
    (unpack path)
    (unpack controller)
    (unpack action)
    0


requestLineParser :: Parser (Text, Text)
requestLineParser = do
  (string "Started ")
  verb <- takeTill (==' ')
  space
  char '"'
  path <- takeTill (=='"')
  skipWhile (not . isEndOfLine )
  endOfLine
  return (verb, path)

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


