module CClient.Util  ( readDate
                  , parseDate
                  , parsePaddedMonthYear
                  , parseUnpaddedMonthYear
                  , parseDOB
                  , renderMonthYear
                  , renderDate
                  , renderDOB
                  , dateToMonthYearNumber
                  , monthYearNumberToDate
                  , readMaybe
                  , first
                  , second
                  , third
                  , showMaybe
                  ) where

import Data.Time (parseTime, readTime, parseTime, Day)
import qualified Data.ByteString.Char8 as BS
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

parseDate :: BS.ByteString -> Maybe Day
parseDate = parseTime defaultTimeLocale "%-m/%-d/%y" . BS.unpack

{- 8/10/13 0:00 -}
readDate :: BS.ByteString -> Day
readDate = readTime defaultTimeLocale "%-m/%d/%y %h:%M" . BS.unpack

parseMonthYear :: String -> BS.ByteString -> Maybe Day
parseMonthYear format = parseTime defaultTimeLocale format . BS.unpack

parsePaddedMonthYear :: BS.ByteString -> Maybe Day
parsePaddedMonthYear = parseMonthYear "%m%Y"

parseUnpaddedMonthYear :: BS.ByteString -> Maybe Day
parseUnpaddedMonthYear = parseMonthYear "%-m%Y"

parseDOB :: String -> Maybe Day
parseDOB = parseTime defaultTimeLocale "%Y%m%d"


renderMonthYear :: Day -> BS.ByteString
renderMonthYear = BS.pack . formatTime defaultTimeLocale "%m%Y"

renderDate :: Day -> BS.ByteString
renderDate = BS.pack . formatTime defaultTimeLocale "%-m/%-d/%y"

renderDOB :: Day -> BS.ByteString
renderDOB = BS.pack . formatTime defaultTimeLocale "%Y%m%d"

dateToMonthYearNumber :: Day -> Int
dateToMonthYearNumber month = 
  let (y, m, _) = toGregorian month in
  fromIntegral y * 12 + fromIntegral m - 1

monthYearNumberToDate :: Int -> Day
monthYearNumberToDate n = 
  fromGregorian (fromIntegral n `div` 12) ((+) 1 $ fromIntegral $ n `mod` 12) 1

readMaybe :: (Read a) => String -> Maybe a
readMaybe s =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing

showMaybe :: (Show a) => Maybe a -> String
showMaybe x = case x of
                Nothing -> ""
                Just y -> show y

first   :: (a,a,a) -> a
first   (x, _, _) = x

second  :: (a,a,a) -> a
second  (_, x, _) = x

third   :: (a,a,a) -> a
third   (_, _, x) = x
