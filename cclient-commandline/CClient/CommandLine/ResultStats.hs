{-# LANGUAGE OverloadedStrings #-}
module CClient.CommandLine.ResultStats where

import Data.Aeson 
import qualified Data.ByteString.Char8 as BS

import CClient.CommandLine.Types
import CClient.CommandLine.Response

data ResultStats = ResultStats{
    rUrl            :: Url
  , rResponseTypes  :: [ResponseType]
  } deriving (Show, Eq)

instance ToJSON ResultStats where
  toJSON (ResultStats url responseTypes) = 
    object  [ "url"           .= url
            , "responseTypes" .= responseTypes
            ]

data ResponseType = ResponseType{
    rtResponse  :: Response
  , rtCount     :: Int
  } deriving (Show, Eq)

instance ToJSON ResponseType where
  toJSON (ResponseType response count) = 
    object  [ "response"  .= response
            , "count"     .= count
            ]
