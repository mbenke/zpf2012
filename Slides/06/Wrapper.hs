-- Za http://blog.sigfpe.com/2007/04/trivial-monad.html
module Wrapper where

newtype W a = W a deriving Show

instance Functor W where
  -- fmap :: (a -> b) -> W a -> W b
  fmap f (W a) = W (f a)

class Pointed f where
  pure :: a -> f a
  
instance Pointed W where
  pure = W
  
a, b :: W Int
a = pure 1
b = fmap (+1) a
-- zapakowaną wartość możemy wielokrotnie zwiększać:
s = fmap (+1)
t = s(s(a))

f :: Int -> W Int
f x = W (x+1)
-- Jak zastosować f dwukrotnie?

bind :: (a -> W b) -> (W a -> W b)
bind f (W a) = f a

c = bind f (f 1)

instance Monad W where
  return = W
  (W x) >>= f = f x
  
g :: Int -> W Int -> W Int  -- g x (W y) = W (x+y), ale bez rozpakowywania
g x wy = undefined

h :: W Int -> W Int -> W Int --h (W x) (W y) = W (x+y), bez rozpakowywania
h wx wy = undefined

-- Udowodnij, że W spełnia prawa monadyki

join :: W (W a) -> W a -- bez rozpakowywania, tylko return i bind
join wwa = undefined