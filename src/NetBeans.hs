module NetBeans where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.MVar      (MVar, putMVar, takeMVar)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, isEmptyTChan,
                                               newTChan, readTChan,
                                               tryReadTChan, writeTChan)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Reader         (ask, runReaderT)
import           Control.Monad.Trans          (liftIO)
import           Network                      (PortID (PortNumber))
import           Vim.Netbeans                 (runForkedNetbeans, Event (..), NetbeansCallbacks(..), nextEvent, Netbeans, editFile)

import qualified Vim.Netbeans.Protocol        as P

import           Types.API                    (Action (..))



netBeans :: MVar String -> MVar Action -> IO ()
netBeans incoming outgoing = runForkedNetbeans
            (PortNumber 4444)
            "password"
            (NetbeansCallbacks
                (liftIO $ putStrLn "Waiting for connection")
                (\e -> case e of
                    StartupDone -> liftIO $ putStrLn "Startup done"
                    _ -> liftIO $ putStrLn "other event"
                )
            )
            [handleOutgoing outgoing]
            (handleIncoming incoming)


    where
      handleIncoming :: MonadIO m => MVar String -> Netbeans m ()
      handleIncoming incoming = do
        ne <- nextEvent
        case ne of
            (_, Disconnect) -> return ()
            (_, e) -> do
              case e of
                FileOpened filename _ _ -> do
                  liftIO $ putStrLn $ "FileOpened filename: " ++ filename
                  liftIO $ putMVar incoming filename
                  handleIncoming incoming
                _ -> do
                  liftIO $ putStrLn $ "unknown event " ++ (show e)
                  handleIncoming incoming

      handleOutgoing :: MVar Action -> Netbeans IO ()
      handleOutgoing outgoing = do
        action <- liftIO $ takeMVar outgoing
        case action of
          OpenFile filename -> do
            liftIO $ putStrLn $ "opening file: " ++ filename
            editFile filename
            handleOutgoing outgoing
          _ -> do
            liftIO $ putStrLn $ "Unknown action: " ++ (show action)
            handleOutgoing outgoing

