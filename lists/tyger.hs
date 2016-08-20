module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

-- This is inefficient -- it looks at each character twice
myLines :: String -> [String]
myLines "" = []
myLines s = takeWhile (/='\n') s : myLines (drop 1 (dropWhile (/='\n') s))

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? "
    ++ show (myLines sentences == shouldEqual)
