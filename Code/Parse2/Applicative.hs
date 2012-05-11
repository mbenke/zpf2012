module Applicative where
infixl 3 <|>
infixl 4 <$>, <$
infixl 4 <*>, <*, *>, <**>

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>)  :: f (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> f a -> f b
  -- f <$> x  = fmap f x 
  
-- | Replace the value.  
(<$) :: Functor f => a -> f b -> f a
(<$) = (<$>) . const

-- | A synonym for 'fmap'.
(<$>) :: Functor f => (a -> b) -> f a -> f b
f <$> a = fmap f a

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  
-- | Sequence actions, discarding the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)
 
-- | Sequence actions, discarding the value of the second argument.
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const
 
-- | A variant of '<*>' with the arguments reversed.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c
