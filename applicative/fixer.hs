module FixerUpper where

q1 = const <$> Just "Hello" <*> pure "World"

q2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
