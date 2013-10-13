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

-- {"num_workers": 4, "urls": ["http://google.com"], "num_requests": 2}
-- {"num_workers": 8, "url": "http://localhost:5000/zones/6M-9hJJpyab5ObGOjWVtEw", "num_requests": 1000}

