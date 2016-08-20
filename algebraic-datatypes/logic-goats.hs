{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Problem: Something like tooMany (43, "foo") causes a type error
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

-- Alternatively, you can use newtype, but you have increased verbosity...
newtype IntString = IntString (Int, String)
instance TooMany IntString where
  tooMany (IntString (n, _)) = n > 42

instance TooMany (Int, Int) where
  tooMany (a, b) = a + b > 42

-- Problem: Using this is not fun
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (m, n) = tooMany m && tooMany n
