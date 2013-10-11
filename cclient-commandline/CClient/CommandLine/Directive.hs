{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving, PatternGuards #-}
module CClient.CommandLine.Directive where

import Data.Aeson 

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Char8 as BS

import CClient.CommandLine.Types

instance FromJSON Directive where
  parseJSON (Object v) =
    Directive <$>
        (v .: "num_workers")
    <*> (v .: "urls")
    <*> (v .: "num_requests")
    <*> (v .:? "click_probability")
