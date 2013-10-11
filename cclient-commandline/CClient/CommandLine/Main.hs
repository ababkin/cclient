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
import qualified Network.HTTP as N

import CClient.CommandLine.Types
import CClient.CommandLine.Directive

main :: IO ()
main = do
    contents <- BL.getContents
    case decode contents of
      Just directive -> do
        {- let directive = Directive 4 ["http://google.com", "http://yahoo.com", "http://microsoft.com", "http://somefakyurl.com"] -}
        let k = dNumWorkers directive
        let urls = dUrls directive
        let n = length urls

        -- count of completed requests
        completedCount <- newTVarIO (0 :: Int)

        -- for reporting results
        results <- newTChanIO

        -- for sending jobs to workers
        jobQueue <- newTChanIO

        -- the number of workers currently running
        workers <- newTVarIO k

        -- one thread reports completed requests to stdout
        forkIO $ writeCompleted results

        -- start worker threads
        forkTimes k workers (worker results jobQueue completedCount)

        atomically $ enqueueJobs jobQueue urls

        -- enqueue "please finish" messages
        atomically $ replicateM_ k (writeTChan jobQueue Done)

        waitFor workers

        numCompleted <- atomically $ readTVar completedCount

        printf fmt numCompleted
                   {- (linksFound stats) -}
                   {- (S.size (linksSeen stats)) -}
                   {- n -}
      Nothing -> do
        putStrLn $ "Cannot parse directive: " ++ (BL.unpack contents)
  where
    fmt   = "Hit %d urls.\n"

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k liveWorkers act =
  replicateM_ k . forkIO $
    act
    `finally`
    (atomically $ modifyTVar_ liveWorkers (subtract 1))

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

writeCompleted :: TChan String -> IO ()
writeCompleted results =
  forever $
    atomically (readTChan results) >>= putStrLn >> hFlush stdout

waitFor :: TVar Int -> IO ()
waitFor liveWorkers = atomically $ do
  count <- readTVar liveWorkers
  check (count == 0)

enqueueJobs :: TChan Task -> [Url] -> STM ()
enqueueJobs jobQueue = mapM_ (writeTChan jobQueue . Get)

worker :: TChan String -> TChan Task -> TVar Int -> IO ()
worker results jobQueue completedCount = loop
  where
    -- Consume jobs until we are told to exit.
    loop = do
        job <- atomically $ readTChan jobQueue
        case job of
            Done  -> return ()
            Get url -> get (BL.unpack url) >> loop

    get url = do
        body <- N.getResponseBody =<< N.simpleHTTP (N.getRequest url)
        report $ "Done: " ++ url ++ ": " ++ body
      where
        report s = atomically $ do
                    modifyTVar_ completedCount (+1)
                    writeTChan results s
