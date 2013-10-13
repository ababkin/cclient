{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards #-}
module CClient.CommandLine.Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
{- import Control.Exception (catch, finally) -}
import Control.Exception (finally)
import Control.Monad.Error
{- import Control.Monad.State -}
{- import Data.Char (isControl) -}
{- import Data.List (nub) -}
{- import Network.URI -}
{- import Prelude hiding (catch) -}
{- import System.Console.GetOpt -}
{- import System.Environment (getArgs) -}
{- import System.Exit (ExitCode(..), exitWith) -}
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BL
{- import qualified Data.Set as S -}
import Data.Aeson (encode, decode)

-- This requires the HTTP package, which is not bundled with GHC

import CClient.CommandLine.Types
import CClient.CommandLine.Directive
import CClient.CommandLine.ResultStats
import CClient.CommandLine.Worker

main :: IO ()
main = do
    contents <- BL.getContents
    case decode contents of
      Just directive -> do
        let k     = dNumWorkers directive
        let url   = dUrl directive
        let times = dTimes directive

        -- count of completed requests
        completedCount <- newTVarIO (0 :: Int)

        -- for reporting results
        results <- newTChanIO

        -- for sending jobs to workers
        jobQueue <- newTChanIO

        -- the accumulated results
        responseStats <- newTVarIO []

        -- the number of workers currently running
        workers <- newTVarIO k

        -- one thread reports completed requests to stdout
        {- forkIO $ writeCompleted results -}

        -- start worker threads
        forkTimes k workers (worker responseStats results jobQueue completedCount)

        atomically $ enqueueJobs jobQueue $ replicate times url

        -- enqueue "please finish" messages
        atomically $ replicateM_ k (writeTChan jobQueue Done)

        finalize workers responseStats

        {- numCompleted <- atomically $ readTVar completedCount -}

      Nothing -> do
        putStrLn $ "Cannot parse directive: " ++ (BL.unpack contents)


forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k liveWorkers act =
  replicateM_ k . forkIO $
    act `finally` (finalizeWorker liveWorkers)

finalizeWorker :: TVar Int -> IO ()
finalizeWorker liveWorkers = atomically $ modifyTVar_ liveWorkers (subtract 1)

{- writeCompleted :: TChan String -> IO () -}
{- writeCompleted results = -}
  {- forever $ -}
    {- atomically (readTChan results) >>= putStrLn >> hFlush stdout -}

finalize :: TVar Int -> TVar [ResponseType] -> IO ()
finalize liveWorkers responseTypes = 
    (BL.putStr . encode) =<< (atomically $ waitFor liveWorkers >> readTVar responseTypes)
  where
    waitFor :: TVar Int -> STM ()
    waitFor liveWorkers = do
      count <- readTVar liveWorkers
      check (count == 0)

enqueueJobs :: TChan Task -> [Url] -> STM ()
enqueueJobs jobQueue = mapM_ (writeTChan jobQueue . Get)



