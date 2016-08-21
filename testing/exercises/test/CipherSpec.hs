module CipherSpec where

import Test.Hspec
import Test.QuickCheck

import Cipher

lowercase :: Gen Char
lowercase = elements ['a'..'z']

lowercaseString :: Gen String
lowercaseString = listOf1 lowercase

spec :: Spec
spec = describe "Cipher" $ do
  describe "caesar" $ do
    it "should reverse itself" $ do
      property $ do
        n <- (arbitrary :: Gen Int)
        x <- lowercaseString
        return $ x == caesarDecode n (caesarEncode n x)
  describe "vigenere" $ do
    it "should reverse itself" $ do
      property $ do
        key <- lowercaseString
        text <- lowercaseString
        return $ text == vigenereDecode key (vigenereEncode key text)
