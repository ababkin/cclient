{-# LANGUAGE ScopedTypeVariables #-}
module CClient.CommandLine.Worker where

import Control.Concurrent.STM
import qualified Network.HTTP as N
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Text.Regex.Posix
import Data.Aeson (encode, decode)
import Data.List
import Data.Time.Clock

import CClient.CommandLine.Types
import CClient.CommandLine.Response
import CClient.CommandLine.ResultStats

worker :: TVar [ResponseType] -> TChan String -> TChan Task -> TVar Int -> IO ()
worker responseTypes results jobQueue completedCount = loop
  where
    loop = do
        job <- atomically $ readTChan jobQueue
        case job of
            Done  -> return ()
            Get url -> get (BS.unpack url) >> loop

    get url = do
        beforeTime <- fmap utctDayTime getCurrentTime
        result <- N.simpleHTTP (N.getRequest url)
        json <- fmap stripJsonP $ N.getResponseBody result
        case decode $ BL.fromChunks [(BS.pack json)] of
          Just (response :: Response) -> do
            updateResponseTypes response beforeTime
            report $ "Done: url: " ++ url ++ " , json: " ++ json
          Nothing ->
            report $ "Error: could not parse json, url: " ++ url ++ " , json: " ++ json

      where
        updateResponseTypes response beforeTime = do
          afterTime <- fmap utctDayTime getCurrentTime
          atomically $ do
            acc <- readTVar responseTypes
            let (matched, not_matched) = partition (\rt -> response == rtResponse rt) acc
            let (old_count, old_time) = case matched of
                              [] -> (0, fromIntegral 0)
                              [match] -> (rtCount match, rtTime match)
            writeTVar responseTypes $ (ResponseType response (old_count+1) (old_time+(afterTime - beforeTime))):not_matched

          
        report s = atomically $ do
                    modifyTVar_ completedCount (+1)
                    writeTChan results s

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

stripJsonP :: String -> String
stripJsonP jsnopBody = jsnopBody =~ "\\{.*\\}" :: String
