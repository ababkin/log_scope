{-# LANGUAGE OverloadedStrings #-}

module Types.RailsEvent where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson

data RailsEvent = StartController {
    controller :: String
  , action     :: String
  , method     :: String
  , path       :: String
  , format     :: String
  , timestamp  :: String
  , source     :: String
  } |
  FinishController {
    controller :: String
  , action     :: String
  , method     :: String
  , path       :: String
  , format     :: String
  , timestamp  :: String
  , source     :: String
  , status     :: Int
  } | Unknown {
    event_type :: String
  , timestamp  :: String
  , source     :: String
  } deriving (Eq, Show)

instance FromJSON RailsEvent where
  parseJSON (Object v) = do
    eventType       <- v .: "event_type"
    payload         <- v .: "payload"
    case eventType of
      "start_processing.action_controller" ->
        StartController <$>
              payload .: "controller"
          <*> payload .: "action"
          <*> payload .: "method"
          <*> payload .: "path"
          <*> payload .: "format"
          <*> v .: "timestamp"
          <*> v .: "source_type"
      "process_action.action_controller" ->
        FinishController <$>
              payload .: "controller"
          <*> payload .: "action"
          <*> payload .: "method"
          <*> payload .: "path"
          <*> payload .: "format"
          <*> v .: "timestamp"
          <*> v .: "source_type"
          <*> payload .: "status"
      unknown ->
        (Unknown unknown) <$>
              v .: "timestamp"
          <*> v .: "source_type"

