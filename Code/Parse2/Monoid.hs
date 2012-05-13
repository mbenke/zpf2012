module Monoid where

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

instance Monoid b => Monoid (a -> b) where
  mempty = const mempty
  mappend f g = \x ->f x `mappend` g x