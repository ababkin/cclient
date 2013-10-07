{-# LANGUAGE OverloadedStrings #-}

module CClient.Types ( MRecord(..)
                  ) where

import Data.Time (Day)
import qualified Data.ByteString.Char8 as BS

data MRecord = MRecord
    { mId               :: Integer
    , mNPI              :: BS.ByteString
    , mClassificationSpecialization  :: BS.ByteString
    , mMSA_PATIENT_KEY   :: BS.ByteString
    , mPATIENT_DOB       :: BS.ByteString
    , mPATIENT_GENDER    :: BS.ByteString
    , mPATIENT_STATE     :: BS.ByteString
    , mICD9Code          :: BS.ByteString
    , mDx                :: BS.ByteString
    , mOrderTestCode     :: BS.ByteString
    , mResultCode        :: BS.ByteString
    , mObserveDateTime   :: Maybe Day
    , mDate              :: BS.ByteString
    , mObserveValue      :: Maybe Double
    } deriving (Show)

