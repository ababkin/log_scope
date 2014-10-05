module Unit.Parsers (logParserUnitTests) where

import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit (testCase, (@?=))

logParserUnitTests = undefined
{- logParserUnitTests :: [TestTree] -}
{- logParserUnitTests = map (\(log, eitherRequest, description) -> -}
  {- testCase description $ -}
  {- requestParser s @?= eitherRequest) allTestCases -}
    {- where -}

      {- allTestCases = concat [ -}
          {- simpleTestCases -}
        {- ] -}

      {- simpleTestCases = [ -}
          {- ("release/1.23.5\n",      [ReleaseTag $ SemVer 1 23 5],                   "single release tag") -}
        {- ] -}
