module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 (zip x y)

ys :: Maybe Integer
ys = lookup 6 (zip y z)

zs :: Maybe Integer
zs = lookup 4 (zip x y)

z' :: Integer -> Maybe Integer
z' = (flip lookup) (zip x z)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- This is not great
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 x = (a, a) where a = z' x

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ s'
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequA 7

  print $ foldr (&&) True (sequA 7)
  print $ foldr (&&) True (sequA 6)

  print $ sequA (fromMaybe undefined s')

  print $ bolt (fromMaybe undefined ys)
