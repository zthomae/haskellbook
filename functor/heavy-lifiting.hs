module HeavyLifting where

-- 1
a :: [Int]
a = fmap (+1) $ read "[1]"

-- 2
b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c :: Int -> Int
c = fmap (*2) (\x -> x - 2)

-- 4
d :: Int -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read (fmap ("123"++) $ fmap show ioi)
    in fmap (*3) changed

main :: IO ()
main = do
  putStr "Expecting [2], got: "
  print a

  putStr "Expecting Just [\"Hi,lol\",\"Hellolol\"], got: "
  print b

  putStr "Expecting -2, got: "
  print (c 1)

  putStr "Expecting \"1[0,1,2,3]\", got: "
  print (d 0)

  putStr "Expecting 3693, got: "
  x <- e
  print x
