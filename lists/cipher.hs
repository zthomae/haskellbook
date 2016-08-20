module Cipher where

import Data.Char

-- Note: only works on lowercase letters

encode :: Int -> Char -> Char
encode n c = chr $ ord 'a' + (ord c + n - ord 'a') `mod` 26

caesarCipher :: Int -> String -> String
caesarCipher n = map (encode n)
