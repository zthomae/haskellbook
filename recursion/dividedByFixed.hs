module DividedByFixed where

-- they recommend a special data type for this, but I already
-- know how to use Maybe

-- first attempt: very ugly, but conforms to divMod
dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy 0 _ = Just (0, 0)
dividedBy num denom
  | num > 0 && denom > 0 =
    let go n d count
         | n < d = Just (count, n)
         | otherwise = go (n - d) d (count + 1)
    in  go num denom 0
  | num < 0 && denom > 0 =
    let go n d count
         | n > 0 = Just (count, n)
         | otherwise = go (n + d) d (count - 1)
    in  go num denom 0
  | num > 0 && denom < 0 =
    let go n d count
         | n < 0 = Just (count, n)
         | otherwise = go (n + d) d (count - 1)
    in go num denom 0
  | otherwise =
    let go n d count
         | n > d = Just (count, n)
         | otherwise = go (n - d) d (count + 1)
    in go num denom 0

-- things that stand out:
-- 1) n - (signum num * signum denom) * d
-- 2) count + (signum num * signum denom)
-- 3) Everything else is identical
-- 4) d is a useless argument

-- I don't like how the conditions are asymmetrical
-- between comparing n with d and n with 0, but I
-- like how this makes the base case elegant...

dividedBy' :: Integral a => a -> a -> Maybe (a, a)
dividedBy' _ 0 = Nothing
dividedBy' 0 _ = Just (0, 0)
dividedBy' num denom =
  let s = signum num * signum denom
      done = case (num > 0, denom > 0) of
                    (True, True) -> (<)
                    (False, True) -> \ n _ -> n > 0
                    (True, False) -> \ n _ -> n < 0
                    (False, False) -> (>)
      go n count
       | done n denom = Just (count, n)
       | otherwise = go (n - s * denom) (count + s)
  in go num 0
