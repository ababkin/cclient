{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards #-}
module CClient.CommandLine.Directive where

import Data.Aeson 

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Char8 as BS

import CClient.CommandLine.Types

data Directive = Directive{
    dNumWorkers :: Int
  , dUrl        :: Url
  , dTimes      :: Int
  , dClickProb  :: Maybe Double
  } deriving (Show)

instance FromJSON Directive where
  parseJSON (Object v) =
    Directive <$>
        (v .:   "num_workers")
    <*> (v .:   "url")
    <*> (v .:   "num_requests")
    <*> (v .:?  "click_probability")

-- {"num_workers": 8, "url": "http://localhost:5000/zones/cGXqt9j9fHdTdM6IpjbOpw", "num_requests": 1000}

