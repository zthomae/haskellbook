module DList where

import Criterion.Main
import qualified Data.DList as RealDL

-- key idea: unDL is an operation that prepends to a list
newtype DList a = DL { unDL :: [a] -> [a] }

-- "empty" means that this operation adds nothing to the realized list. if empty is a "leaf"
-- in the computation, it will only ever be passed [] -- in this case, DList (\_ -> []) would
-- also do the same thing, albeit less elegantly
empty :: DList a
empty = DL id
{-# INLINE empty #-}

-- "singleton" creates a new DList out of a value. it does this by (list) consing
-- this value onto whatever list is used to realize the DList. example: toList (singleton 1)
-- is just [1]. if singleton is a "leaf" in the computation, it will only ever be passed [] --
-- in this case, DList (\_ -> [a]) would also do the same thing, albeit less elegantly
singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

-- toList forces the realization of the list by passing an initial seed value of []
toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

-- cons takes a value to prepend and a DList. it creates a new DList with a function
-- that uses its list argument to realize the DList parameter and then prepends the
-- parameter value in front of it
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- snoc takes a DList to append to and a value. it creates a new DList with a function
-- that realizes the DList parameter using the value parameter consed onto the list
-- argument
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- append takes two DList and appends them together. it creates a new DList with
-- a function that realizes the _second_ DList using the list argument, with
-- the list resulting from this used to realize the _first_ DList
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) (n:xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (singleton n `append` xs)

-- I didn't notice the speed increase promised by the book, so I've written
-- the above benchmark using the dlist library to help show myself that I was
-- doing it correctly
constructRealDlist :: Int -> [Int]
constructRealDlist i = RealDL.toList $ go i RealDL.empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (RealDL.singleton n `RealDL.append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDlist 123456
  , bench "concat real dlist" $
    whnf constructRealDlist 123456
  ]
