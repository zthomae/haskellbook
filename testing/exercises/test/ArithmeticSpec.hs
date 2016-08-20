module ArithmeticSpec where

import Test.Hspec
import Test.QuickCheck

import Data.List (sort)

import Arithmetic

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

multiplicationAssociative x y z = x * (y * z) == (x * y) * z
multiplicationCommutative x y = x * y == y * x

quotRemProp x y = (quot x y) * y + (rem x y) == x
divModProp x y = (div x y ) * y + (mod x  y) == x

spec :: Spec
spec = describe "ArithmeticSpec" $ do
  describe "half" $ do
    it "half should divide by 2" $ do
      property $ \x -> halfIdentity x == (x :: Double)
  describe "sort" $ do
    it "should sort a list into order" $ do
      property $ \lst -> listOrdered (sort (lst :: [Integer]))
  describe "addition" $ do
    it "should be associative" $ do
      property (plusAssociative :: Integer -> Integer -> Integer -> Bool)
    it "should be commutative" $ do
      property (plusCommutative :: Integer -> Integer -> Bool)
  describe "multiplication" $ do
    it "should be associative" $ do
      property (multiplicationAssociative :: Integer -> Integer -> Integer -> Bool)
    it "should be commutative" $ do
      property (multiplicationCommutative :: Integer -> Integer -> Bool)
  describe "quotRem" $ do
    it "should follow the law" $ do
      property $ do
        NonZero a <- arbitrary :: Gen (NonZero Integer)
        NonZero b <- arbitrary :: Gen (NonZero Integer)
        return $ quotRemProp a b
  describe "divMod" $ do
    it "should follow the law" $do
      property $ do
        NonZero a <- arbitrary :: Gen (NonZero Integer)
        NonZero b <- arbitrary :: Gen (NonZero Integer)
        return $ divModProp a b
  describe "reverse" $ do
    it "should do the right thing" $ do
      property $ \ lst -> (reverse . reverse) lst == id (lst :: [Integer])
