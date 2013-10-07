module CClient.Process where

import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Monad.Trans
{- import Control.Monad.Reader (ReaderT, Reader, asks) -}
import Data.Functor ((<$>))
import Control.Monad.Reader (Reader, asks)
import Data.Maybe (fromJust)

import CClient.Types
import CClient.Util  ( dateToMonthYearNumber
                  , parsePaddedMonthYear
                  , renderMonthYear
                  )


{- filterByLoinc :: Maybe PurchaseRecord -> Reader QueryParams (Maybe PurchaseRecord) -}
{- filterByLoinc (Just rec) = do -}
                            {- maybeLoincId <- asks qLoinc -}
                            {- return $ case maybeLoincId of -}
                                      {- Just loincId -> -}
                                        {- if (iObserveId rec) == loincId -}
                                                   {- then Just rec  -}
                                                   {- else Nothing -}
                                      {- Nothing -> Just rec -}

{- filterByLoinc Nothing = return Nothing -}

filterByMonthRange :: Maybe PurchaseRecord -> Reader QueryParams (Maybe PurchaseRecord)
filterByMonthRange (Just rec) = do
                          maybeStartDate  <- do
                                              maybeMonthYear <- asks qFirstMonth
                                              return $ parsePaddedMonthYear =<< maybeMonthYear

                          maybeEndDate    <- do
                                              maybeMonthYear <- asks qLastMonth
                                              return $ parsePaddedMonthYear =<< maybeMonthYear

                          return $ case pDate rec of
                                     Just date -> if (maybe True (\start -> date >= start) maybeStartDate)
                                                  && (maybe True (\end -> date <= end) maybeEndDate)
                                                    then Just rec
                                                    else Nothing
                                     Nothing -> Nothing
filterByMonthRange Nothing = return Nothing


filterRecord :: PurchaseRecord -> Reader QueryParams (Maybe PurchaseRecord)
{- filterRecord rec = filterByLoinc =<< filterByMonthRange =<< (return $ Just rec) -}
filterRecord rec = filterByMonthRange =<< (return $ Just rec)

{- filterAndGroupByMsaPatientKey :: V.Vector PurchaseRecord -> Reader QueryParams (Map.Map Int (V.Vector PurchaseRecord)) -}
{- filterAndGroupByMsaPatientKey = V.foldM' (\mp r -> do -}
                                                    {- let pkey = iMsaPatientKey r -}
                                                    {- let insert m key value = Map.insertWith (V.++) key (V.singleton value) m -}
                                                    {- let maybeInsert m = maybe m (insert m pkey) -}
                                                    {- rec <- filterRecord r -}
                                                    {- return $ maybeInsert mp rec -}
                                {- ) Map.empty -}


filterAndGroupByMonth :: V.Vector PurchaseRecord -> Reader QueryParams (Map.Map MonthYear (V.Vector PurchaseRecord))
filterAndGroupByMonth = V.foldM' (\mp r -> do
                                            let pkey = renderMonthYear $ fromJust $ pDate r
                                            let insert m key value = Map.insertWith (V.++) key (V.singleton value) m
                                            let maybeInsert m = maybe m (insert m pkey)
                                            rec <- filterRecord r
                                            return $ maybeInsert mp rec
                                ) Map.empty


filterAndGroupByState :: V.Vector PurchaseRecord -> Reader QueryParams (Map.Map State (V.Vector PurchaseRecord))
filterAndGroupByState = V.foldM' (\mp r -> do
                                            let pkey = pCustomerState r
                                            let insert m key value = Map.insertWith (V.++) key (V.singleton value) m
                                            let maybeInsert m = maybe m (insert m pkey)
                                            rec <- filterRecord r
                                            return $ maybeInsert mp rec
                                ) Map.empty

{- process :: V.Vector PurchaseRecord -> Reader QueryParams Int -- (Map.Map Int (V.Vector PurchaseRecord)) -- -> Reader QueryParams (V.Vector MOut) -}
{- process v = Map.size <$> filterAndGroupByMsaPatientKey v -}

addAggregate :: V.Vector PurchaseRecord -> Int
addAggregate = V.foldr' ((+) . pRevenue) 0

processByMonth :: V.Vector PurchaseRecord -> Reader QueryParams ByMonthResults -- (Map.Map Int (V.Vector PurchaseRecord)) -- -> Reader QueryParams (V.Vector MOut)
processByMonth v = do
                    byMonthMap <- filterAndGroupByMonth v
                    return $ Map.foldWithKey (\month records acc -> 
                                              ByMonthResult { bmId              = dateToMonthYearNumber $ fromJust $ parsePaddedMonthYear month
                                                            , bmMonth           = month
                                                            , bmTotalPurchases  = (V.length records)
                                                            , bmTotalRevenue    = (addAggregate records)
                                                            } : acc
                                            ) [] byMonthMap

processByState :: V.Vector PurchaseRecord -> Reader QueryParams ByStateResults
processByState v = do
                    byStateMap <- filterAndGroupByState v
                    return $ Map.foldWithKey (\state records acc -> 
                                              ByStateResult { bsId              = state
                                                            , bsState           = state
                                                            , bsTotalPurchases  = (V.length records)
                                                            , bsTotalRevenue    = (V.length records)
                                                            } : acc
                                            ) [] byStateMap

