module Optional where

import Data.Monoid

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Only mempty
  mappend Nada Nada = Nada
  mappend a Nada = a
  mappend Nada b = b
  mappend (Only a) (Only b) = Only (a <> b)
