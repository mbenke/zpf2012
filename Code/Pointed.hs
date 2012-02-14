module Pointed where
import Prelude hiding(Functor(..))
import Data.Char(ord)

class Functor f where
  fmap :: (a->b) -> f a -> f b
instance Functor [] where
  fmap = map

infixl 4 <$>
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

class Functor f => Pointed f where
   pure :: a -> f a

instance Pointed [] where
   pure = (:[])

infixl 4 <*>
class Pointed f => Applicative f where
  (<*>) :: f(a->b) -> f a -> f b 
  (*>) :: f a -> f b -> f b
  x *> y = (flip const) <$> x <*> y
  (<*) :: f a -> f b -> f a
  x <* y = const <$> x <*> y

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

instance Applicative [] where
  -- [a->b] -> [a] -> [b]
  fs <*> xs = concat $ pam fs (pam xs) where
    pam :: [a] -> (a->b) -> [b]
    pam = flip map

pam :: [a] -> (a->b) -> [b]
pam = flip map

fs :: [Char -> Int]
fs = pure ord

xs :: [Char]
xs = "ala"