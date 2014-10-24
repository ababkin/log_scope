{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.UdpServer where

import           Control.Concurrent.MVar    (MVar (..), putMVar)
import           Control.Monad
import           Data.Aeson                 (eitherDecode)
import           Data.Bits
import           Data.List
import           Network.BSD
import           Network.Socket


import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T

import           Types.RailsEvent
import           Types.Request

serveLog :: String              -- ^ Port number or name; 514 is default
         -> (Maybe Request -> String -> IO (Maybe Request))  -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       putStrLn "starting UDP server..."
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock Nothing
    where procMessages sock maybeRequest =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 -- Handle it
                 newMaybeRequest <- handlerfunc maybeRequest msg
                 -- And process more messages
                 procMessages sock newMaybeRequest

-- A simple handler that prints incoming packets
notificationHandler :: MVar Request -> Maybe Request -> String -> IO (Maybe Request)
notificationHandler requests maybeRequest msg = do
  putStrLn msg
  case maybeRequest of
    Nothing -> do
    -- wait for the start of the next request
      case eitherDecode $ BL.pack msg of
        Right (StartController controller action verb path format timestamp _) -> do
          putStrLn "parsed StartController"
          return $ Just $ Request verb path controller action format 0 timestamp [] []
        Right unexpected -> do
          putStrLn $ "parsed unexpected event with no request in progress: " ++ (show unexpected)
          return Nothing
        Left err -> do
          putStrLn $ "failed parsing event: " ++ (show err)
          return Nothing

    Just req -> do
      case eitherDecode $ BL.pack msg of
        Right (FinishController _ _ _ _ _ _ _ sc) -> do
          putStrLn "parsed FinishController"
          putMVar requests $ req{statusCode = sc}
          return Nothing
        Right (RenderPartial path timestamp duration) -> do
          return $ Just req{renderedPartials = (replicate 100 (PartialRendered path timestamp duration)) ++ (renderedPartials req)}
          {- return $ Just req{renderedPartials = (PartialRendered path timestamp duration):(renderedPartials req)} -}
        Right (Sql timestamp duration) -> do
          return $ Just req{sqlQueries = replicate 35 $ SqlQuery "" "" 0}
          {- return $ Just req{sqlQueries = [SqlQuery "joan_rivers/style_guide/components/_main_navigation" "" 0]} -}
          {- return $ Just req{sqlQueries = (SqlQuery "SELECT \"expertises\".*" "1.45ms" 0):(sqlQueries req)} -}
        unexpected -> do
          putStrLn $ "parsed unexpected event in the middle of request: " ++ (show unexpected)
          return maybeRequest

  {- case eitherDecode (BL.pack msg) :: Either String Notification of -}
    {- Right n -> -}
      {- putMVar requests n -}

    {- Left s -> do -}
      {- putStrLn $ "Cannot parse notification: " ++ msg -}
      {- putStrLn $ "Error: " ++ s ++ "\n" -}

  {- putStrLn $ "From " ++ show addr ++ ": " ++ msg   -}



startUdpServer :: MVar Request -> IO ()
startUdpServer requests = do
  serveLog "5555" $ notificationHandler requests
