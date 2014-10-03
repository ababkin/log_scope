{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module UdpServer where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Monad
import Data.Aeson (eitherDecode)
import           Control.Concurrent.MVar (MVar (..), putMVar)


import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BL

import Types
import Notification

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
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
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, _, addr) <- recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
notificationHandler :: MVar Request -> HandlerFunc
notificationHandler requests addr msg = do
  putStrLn $ show msg
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
