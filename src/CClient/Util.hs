module CClient.Util  (
    readMaybe
  , showMaybe
  , first
  , second
  , third
  ) where


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
