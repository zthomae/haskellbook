module Hangman where

data Puzzle = Puzzle { puzzleWord :: String
                     , puzzleFilled :: [Maybe Char]
                     , puzzleGuesses :: [Char]
                     } deriving (Eq, Show)

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = elem char word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char guessed

-- removing the IO to make this simpler

handleGuess :: Puzzle -> Char -> Puzzle
handleGuess puzzle guess = do
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> puzzle
    (True, _) -> fillInCharacter puzzle guess
    (False, _) -> fillInCharacter puzzle guess
