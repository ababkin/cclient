module CClient.CommandLine.Types where

import qualified Data.ByteString.Char8 as BS

type Url          = BS.ByteString
type Uuid         = BS.ByteString
type TextAppend   = BS.ByteString
type Probability  = Double

data Task = Get Url Probability | Done




