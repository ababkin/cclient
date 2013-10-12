{-# LANGUAGE ScopedTypeVariables #-}
module CClient.CommandLine.Worker where

import Control.Concurrent.STM
import qualified Network.HTTP as N
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Text.Regex.Posix
import Data.Aeson (encode, decode)
import Data.List

import CClient.CommandLine.Types
import CClient.CommandLine.Response
import CClient.CommandLine.ResultStats

worker :: TVar [ResponseType] -> TChan String -> TChan Task -> TVar Int -> IO ()
worker responseTypes results jobQueue completedCount = loop
  where
    -- Consume jobs until we are told to exit.
    loop = do
        job <- atomically $ readTChan jobQueue
        case job of
            Done  -> return ()
            Get url -> get (BS.unpack url) >> loop

    get url = do
        result <- N.simpleHTTP (N.getRequest url)
        json <- fmap stripJsonP $ N.getResponseBody result
        case decode $ BL.fromChunks [(BS.pack json)] of
          Just (response :: Response) -> do
            atomically $ do
              acc <- readTVar responseTypes

              let (matched, not_matched) = partition (\rt -> response == rtResponse rt) acc
              let old_count = case matched of
                                [] -> 0
                                [match] -> rtCount match

              writeTVar responseTypes $ (ResponseType response (old_count+1)):not_matched
                
            report $ "Done: url: " ++ url ++ " , json: " ++ json
          Nothing ->
            report $ "Error: could not parse json, url: " ++ url ++ " , json: " ++ json

      where
        report s = atomically $ do
                    modifyTVar_ completedCount (+1)
                    writeTChan results s

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

stripJsonP :: String -> String
stripJsonP jsnopBody = jsnopBody =~ "{.*}" :: String
{- localJsonpCallback( -}
