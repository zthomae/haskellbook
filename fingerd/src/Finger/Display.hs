{-# LANGUAGE OverloadedStrings #-}

module Finger.Display where

import Data.Text (Text)
import qualified Data.Text as T

import Finger.Model (User(..))

columnStart :: Int -> Int -> Int -> Int
columnStart target gapSize len =
  let
    minCol = max target len
  in
    if minCol - len < gapSize
    then minCol + 8
    else minCol

spacesAfter :: Int -> Int -> Int -> Int -> (Int, Int)
spacesAfter target gapSize first second
    | first < second =
      let (a, b) = spacesAfter target gapSize second first in (b, a)
    | otherwise =
      let
        start = columnStart target gapSize first
      in
        (start - first, start - second)

tabsWithPadding :: Int -> Text
tabsWithPadding len = T.concat
  [ T.replicate (len `mod` 8) " "
  , T.replicate (len `div` 8) "\t"
  ]

formatUser :: User -> Text
formatUser user =
  let
    login = T.concat ["Login: ", username user]
    dir = T.concat ["Directory: ", homeDirectory user]
    (spaceAfterLogin, spaceAfterDir) = spacesAfter 40 8 (T.length login) (T.length dir)
  in
    T.concat
    [ login, tabsWithPadding spaceAfterLogin
    , "Name: ", realName user, "\n"
    , dir, tabsWithPadding spaceAfterDir
    , "Shell: ", shell user, "\n"
    , "Phone: ", phone user, "\n"
    ]
