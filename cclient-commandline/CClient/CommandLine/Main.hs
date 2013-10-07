{-# LANGUAGE OverloadedStrings #-}
module CClient.CommandLine.Main where

import System.Environment (getArgs)

import CClient.CSV ()

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import CClient.Types


main :: IO ()
main = do
  content <- BL.getContents
  BL.putStr content

  {- case decode content of -}
    {- Just directive -> do -}
      {- dispatchDebug $ "parsed directive: " ++ (show directive) -}
    {- Nothing -> do -}
      {- dispatchDebug $ "could not parse directive: " ++ (show content) -}
