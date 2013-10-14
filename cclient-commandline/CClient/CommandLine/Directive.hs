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
    <*> (v .:?  "click_prob")

-- {"num_workers": 8, "url": "http://localhost:5000/zones/6AIiO25vKQq-Z3fBN-1XyQ", "num_requests": 10}
-- {"num_workers": 8, "url": "http://localhost:5000/zones/wp2dChcoP0Ab14PQhsI3Gw", "num_requests": 10, "click_prob": 0.1}

