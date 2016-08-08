module Jammin where

import Data.Bool
import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

data JamJars' =
  Jam' Fruit Int
  deriving (Eq, Ord, Show)

-- 2

data JamJars =
  Jam { jamJarFruit :: Fruit
      , jamJarJars :: Int }
      deriving (Eq, Ord, Show)

-- 3: The cardinality of JamJars is the cardinality of Int (2^64) + the
-- cardinality of Fruit (4)

-- 6

numJars :: [JamJars] -> Int
numJars (j:[]) = jamJarJars j
numJars (j:js) = foldr ((+) . jamJarJars) (jamJarJars j) js

-- 7 -- note simpler pattern for one-element list

mostJars :: [JamJars] -> JamJars
mostJars [j] = j
mostJars (j:js) = foldr (\a b -> bool a b (jamJarJars a > jamJarJars b)) j js

-- 8

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy (\a b -> compare (jamJarJars a) (jamJarJars b))

-- 9

groupAndSortJams :: [JamJars] -> [[JamJars]]
groupAndSortJams = map sortJams . groupBy (\a b -> jamJarFruit a == jamJarFruit b)
