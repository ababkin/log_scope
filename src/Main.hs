{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ((<$>), (<*>), (*>), (<*), (<|>), many)
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Data.Attoparsec.Text (Parser(..), string, takeTill, skipWhile, endOfLine, isEndOfLine)
import           Data.Text

import           Pipes (Producer(..), runEffect, yield, (>->))
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import           Pipes.Parse (runStateT)
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text



data Request = Request {
  verb :: Text
} deriving Show

requestParser :: Parser Request
requestParser =
    Request <$> ((string "Started ") *> (takeTill (==' ')) <* skipWhile (not . isEndOfLine ) <* endOfLine)
  <|>
    skipWhile (not . isEndOfLine ) *> endOfLine *> requestParser


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
