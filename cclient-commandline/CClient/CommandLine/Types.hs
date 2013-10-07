{-# LANGUAGE OverloadedStrings #-}
module CClient.CommandLine.Types where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import CClient.Types ( MRecord(..)
                  )
type Filename         = BS.ByteString

data Directive = Directive  
    { dUrl        :: String
    , dTimes      :: Integer
    , dClickProb  :: Double
    } deriving (Show)



