module FoldableFunctions where

import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' t = getSum $ foldMap Sum t

product' :: (Foldable t, Num a) => t a -> a
product' t = getProduct $ foldMap Product t

elem' :: (Foldable t, Eq a) => t a -> a -> Bool
elem' t x = getAny (foldMap (Any . (==x)) t)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\ x z -> maybe (Just x) (\ e -> Just $ min x e) z) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\ x z -> maybe (Just x) (\ e -> Just $ max x e) z) Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (\ _ _ -> True) False

length' :: (Foldable t) => t a -> Int
length' = foldr (\ _ z -> z + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- fold in terms of foldMap
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = foldr (\ x z -> f x <> z) mempty t
