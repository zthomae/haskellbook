{-# LANGUAGE ViewPatterns #-} -- BLACK MAGIC

module EtcSpec where

import Test.QuickCheck
import Test.QuickCheck.Function

import Data.Char
import Data.List

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genMostlyFulse :: Gen Fool
genMostlyFulse = frequency [(2, return Fulse), (1, return Frue)]

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = (toUpper c):cs

prop_capitalizeIdempotent x =
  let c = capitalizeWord x
  in
    c == twice capitalizeWord x && c == fourTimes capitalizeWord x

prop_sortIdempotent x =
  let sorted = sort x
  in
    sorted == twice sort x && sorted == fourTimes sort x

-- This fails: floating point arithmetic is not precise enough
square x = x * x
prop_squareIdentity x = x == (square . sqrt) x

-- These are equivalent
prop_foldrpp x y = foldr (:) x y == (++) x y

-- Type assertion necessary for this to work, but they are equivalent
prop_foldrconcat lsts = (foldr (++) [] (lsts :: [[Integer]])) == concat lsts

prop_dollar :: Fun Integer Integer -> Integer -> Bool
prop_dollar (apply -> f) a = (f $ a) == (f a)
