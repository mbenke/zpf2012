{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FunctionalDependencies #-}

module CatMonad where
import Prelude hiding(id,(.), Monad(..))
import Data.Array

class Category (~>) where
  id  :: a ~> a
  (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)

instance Category (->) where
  id x = x
  (f . g) x = f (g x)
  
class (Category (~>), Category (~~>)) 
      => Functor' f (~>) (~~>)  | f (~>) -> (~~>), f (~~>) -> (~>)  where
  fmap' :: (a ~> b) -> (f a ~~> f b)

class Category (~>) => Monad' m (~>) where
  return' :: a ~> m a
  bind'   :: (a ~> m b) -> (m a ~> m b)
  
-- 1. bind return = id  
-- 2. bind f . return = f
-- 3. bind f . bind g = bind (bind g . f)

class Functor f => Applicative f where
   pure  ::  a -> f a
-- fmap  :: (a -> b) -> f a -> f b
   (<*>) :: f (a -> b) -> f a -> f b
   
-- fmap g . pure = pure . g
-- fmap g x = pure g <*> x

class Applicative m => Monad'' m where
  join :: m (m a) -> m a

type a :~> b = a -> b

class Functor m => Monad m where
  return :: a :~> m a
  bind   :: (a :~> m b) -> (m a :~> m b)
  
class Functor w => Comonad w where
  extract :: w a :~> a
  extend :: (w b :~> a) -> (w b :~> w a)

(=>>) :: Comonad w => w b -> (w b -> a) -> w a
(=>>) = flip extend

data Pointer i e = P i (Array i e) deriving Show

instance Ix i => Functor (Pointer i) where
   fmap f (P i a) = P i (fmap f a)

instance Ix i => Comonad (Pointer i) where
   extract (P i a) = a!i
   extend f (P i a) = P i $ listArray bds (fmap (f . flip P a) (range bds))
       where bds = bounds a

x = listArray (0,9) [0..9]
wrap i = if i<0 then i+10 else if i>9 then i-10 else i
blur (P i a) = let
       k = wrap (i-1)
       j = wrap (i+1)
   in 0.25*a!k + 0.5*a!i + 0.25*a!j
      
test1 = P 0 x =>> blur
x ==> f = f x
test2 = P 0 x ==> fmap (+1) =>> blur ==> fmap (*2) ==> fmap (^2)
