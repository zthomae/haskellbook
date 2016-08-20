module Db where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

getDbDate :: DatabaseItem -> Maybe UTCTime
getDbDate d = case d of
  DbDate t -> Just t
  _ ->  Nothing


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  let f item times =
        case getDbDate item of
          Just t -> t : times
          _ -> times
  in foldr f []

getDbNumber :: DatabaseItem -> Maybe Integer
getDbNumber d = case d of
  DbNumber i -> Just i
  _ -> Nothing

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  let f item dbnums =
        case getDbNumber item of
          Just i -> i : dbnums
          _ -> dbnums
  in foldr f []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dbs =
  let (t:ts) = filterDbDate dbs
  in foldr max t ts

-- lol no fold
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

average :: Double -> Double -> Double
average a b = a + b / 2

avgDb :: [DatabaseItem] -> Double
avgDb dbs = foldr (average . fromIntegral) 0 (filterDbNumber dbs)
