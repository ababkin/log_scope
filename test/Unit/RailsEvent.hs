module Unit.RailsEvent (railsEventUnitTests) where

import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.IO.Unsafe           (unsafePerformIO)
import           Test.Tasty                 (TestTree)
import           Test.Tasty.HUnit           (testCase, (@?=))

import           RailsEvent

railsEventUnitTests :: [TestTree]
railsEventUnitTests = let
    assert (description, jsonFilename, eitherRailsEvent) = let
        json = BSL.pack $ unsafePerformIO $ readFile $ "test/json/" ++ jsonFilename ++ ".json"
      in
      testCase description $ eitherDecode json @?= eitherRailsEvent
  in
  map assert allTestCases
  where

    allTestCases = [
        ( "simple get request: start controller"

        , "start_processing_action_controller"

        , Right $ StartController
          "AdsController"
          "index"
          "GET"
          "/"
          "html"
          "10:20:02:(259)"
          "rails"
        )
      ]

