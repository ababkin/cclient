{-# LANGUAGE OverloadedStrings #-}
module CClient.CommandLine.ResultStats where

import Data.Aeson 
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock

import CClient.CommandLine.Types
import CClient.CommandLine.Response

data ResultStats = ResultStats{
    rsUrl             :: Url
  , rsResponseTypes   :: [ResponseType]
  , rsTotalTime       :: Double
  } deriving (Show, Eq)

instance ToJSON ResultStats where
  toJSON (ResultStats url responseTypes totalTime) = 
    object  [ "url"           .= url
            , "responseTypes" .= responseTypes
            , "totalTime"     .= totalTime
            ]

data ResponseType = ResponseType{
    rtResponse  :: Response
  , rtCount     :: Int
  , rtTime      :: Double 
  } deriving (Show, Eq)

instance ToJSON ResponseType where
  toJSON (ResponseType response count time) = 
    object  [ "response"  .= response
            , "count"     .= count
            , "time"      .= time
            ]
