module HangmanSpec where

import Test.Hspec

import Hangman

spec :: Spec
spec = describe "Hangman" $ do
  let p = Puzzle { puzzleWord = "asdf"
                 , puzzleFilled = [Just 'a', Nothing, Nothing, Nothing]
                 , puzzleGuesses = "a"
                 }
  describe "fillInCharacter" $ do
    it "should fill in a new letter" $ do
      let n = fillInCharacter p 's'
      puzzleFilled n `shouldBe` [Just 'a', Just 's', Nothing, Nothing]
      all (\c -> elem c $ puzzleGuesses n) "as" `shouldBe` True
    it "should not fill in a bad letter" $ do
      let n = fillInCharacter p 'z'
      puzzleFilled n `shouldBe` puzzleFilled p
      all (\c -> elem c $ puzzleGuesses n) "az" `shouldBe` True

  -- TODO: Go back and test with IO

  describe "handleGuess" $ do
    it "should not fill in a letter twice" $ do
      handleGuess p 'a' `shouldBe` p
    it "should fill in a new letter" $ do
      let n = handleGuess p 's'
      puzzleFilled n `shouldBe` [Just 'a', Just 's', Nothing, Nothing]
      all (\c -> elem c $ puzzleGuesses n) "as" `shouldBe` True
    it "should not fill in a bad letter" $ do
      let n = fillInCharacter p 'z'
      puzzleFilled n `shouldBe` puzzleFilled p
      all (\c -> elem c $ puzzleGuesses n) "az" `shouldBe` True
