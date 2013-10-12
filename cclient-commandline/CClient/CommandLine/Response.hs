{-# LANGUAGE OverloadedStrings #-}
module CClient.CommandLine.Response where

import Data.Aeson 
import qualified Data.ByteString.Char8 as BS
import Control.Applicative ((<$>), (<*>))

import CClient.CommandLine.Types

data Response = Response{
    rUuid       :: Uuid
  , rUrl        :: Url
  , rTextAppend :: Maybe TextAppend
  , rLink       :: Maybe Url
  } deriving (Show, Eq)


instance FromJSON Response where
  parseJSON (Object v) = Response <$>
        (v .:   "uuid")
    <*> (v .:   "url")
    <*> (v .:?  "text_append")
    <*> (v .:?  "link")

instance ToJSON Response where
  toJSON (Response uuid url text_append link) = 
    object  [ "uuid"        .= uuid
            , "url"         .= url
            , "text_append" .= text_append
            , "link"        .= link
            ]
