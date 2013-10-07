{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module CClient.CSV () where

import Control.Applicative ((<*>), (<$>))
{- import Data.Csv (FromNamedRecord(..), parseNamedRecord, (.:)) -}
import Data.Csv 
import Data.Maybe (fromJust)
import Text.Read

import CClient.Util (parseDate, renderDate, showMaybe)
import CClient.Types (MRecord(..))


instance ToNamedRecord MRecord where
     toNamedRecord (MRecord phKey npi cs patKey pDOB pGender pState icd9 dx orderTestCode resultCode observeDateTime date observeValue) = namedRecord [
        "MSA_PHYSICIAN_KEY"             .= phKey
      , "NPI"                           .= npi
      , "ClassificationSpecialization"  .= cs
      , "MSA_PATIENT_KEY"               .= patKey
      , "PATIENT_DOB"                   .= pDOB
      , "PATIENT_GENDER"                .= pGender
      , "PATIENT_STATE"                 .= pState
      , "ICD9Code"                      .= icd9
      , "Dx"                            .= dx
      , "OrderTestCode"                 .= orderTestCode
      , "ResultCode"                    .= resultCode
      , "Observe_DateTime"              .= (renderDate $ fromJust observeDateTime)
      , "Date"                          .= date
      , "Observe_Value"                 .= (showMaybe observeValue)
      ]

instance FromNamedRecord MRecord where
  parseNamedRecord r = MRecord 
    <$> r .: "MSA_PHYSICIAN_KEY"
    <*> r .: "NPI"
    <*> r .: "ClassificationSpecialization"
    <*> r .: "MSA_PATIENT_KEY"
    <*> r .: "PATIENT_DOB"
    <*> r .: "PATIENT_GENDER"
    <*> r .: "PATIENT_STATE"
    <*> r .: "ICD9Code"
    <*> r .: "Dx"
    <*> r .: "OrderTestCode"
    <*> r .: "ResultCode"
    <*> (parseDate <$> r .: "Observe_DateTime")
    <*> r .: "Date"
    <*> (readMaybe <$> r .: "Observe_Value")


