module CClient.CommandLine.Types where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

{- import CClient.Types (  -}
    {- Directive(..) -}
  {- ) -}

type Url = BL.ByteString
data Task = Get Url | Done

data Directive = Directive{
    dNumWorkers :: Int
  , dUrls       :: [Url]
  , dTimes      :: Int
  , dClickProb  :: Maybe Double
  } deriving (Show)


