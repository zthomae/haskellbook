module SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

-- 1 --
myTuples = [(x, y) | x <- mySqr, y <- myCube]

-- if you don't want the Cartesian product, use zip --

-- 2 --
myOtherTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3 --
numberOfOtherTuples = length myOtherTuples
