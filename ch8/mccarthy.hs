module McCarthy91 where

mc91 :: (Num a, Ord a) => a -> a
mc91 n
   | n > 100 = n - 10
   | otherwise = mc91(mc91(n + 11))

main = print (map mc91 [95..110])
