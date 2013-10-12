module CClient.CommandLine.Types where

import qualified Data.ByteString.Char8 as BS

type Url        = BS.ByteString
type Uuid       = BS.ByteString
type TextAppend = BS.ByteString

data Task = Get Url | Done




