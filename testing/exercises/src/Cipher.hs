module Cipher where

import Data.Char
import Data.List

caesarEncode :: Int -> String -> String
caesarEncode n = map (encode n)
  where encode n c = chr $ ord 'a' + (ord c + n - ord 'a') `mod` 26

caesarDecode :: Int -> String -> String
caesarDecode n = caesarEncode (-n)

toChar :: Int -> Char
toChar o = chr (ord 'a' + o)

fromChar :: Char -> Int
fromChar c = ord c - ord 'a'

-- Not splitting into words first
vigenereEncode :: String -> String -> String
vigenereEncode key plaintext = map (toChar . encode) (zip plaintext (cycle key))
  where encode (b, c) =
          let
            b' = fromChar b
            c' = fromChar c
          in
            (b' + c') `mod` 26

vigenereDecode :: String -> String -> String
vigenereDecode key ciphertext = map (toChar . decode) (zip ciphertext (cycle key))
  where decode (b, c) =
          let
            b' = fromChar b
            c' = fromChar c
          in
            (b' - c') `mod` 26