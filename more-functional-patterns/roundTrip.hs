module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main = do
  print (roundTrip 4)
  print (roundTrip2 4 :: Int)
  print (id 4)
