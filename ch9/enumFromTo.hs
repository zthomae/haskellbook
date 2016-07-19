module EnumFromTo where

-- One way to do this (but the other way is better)
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []
eftBool b _ = [b]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b = case compare a b of
  GT -> []
  EQ -> [a]
  LT -> a : eftOrd (succ a) b

eftInt :: Int -> Int -> [Int]
eftInt a b = case compare a b of
  GT -> []
  EQ -> [a]
  LT -> a : eftInt (succ a) b

eftChar :: Char -> Char -> [Char]
eftChar a b = case compare a b of
  GT -> []
  EQ -> [a]
  LT -> a : eftChar (succ a) b
