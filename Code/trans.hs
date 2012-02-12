{-#LANGUAGE KindSignatures, ExplicitForAll #-}
{-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
g :: forall b.b->b
g = id

class Functor f => Pointed (f :: * -> *) where
        pure :: forall (a :: *).a-> f a

class MonadTrans (t :: (* -> *) -> * -> *) where
    lift :: Monad (m :: * -> *) => forall (a :: *).m a -> t m a

class Trans a b where
      trans :: a -> b

instance Trans Int Integer where
      trans = toInteger
      
class Iso a b where
      iso :: a -> b
      osi :: b -> a
      
instance Iso a a where
      iso = id
      osi = id
      
instance Iso ((a,b)->c) (a->b->c) where
      iso = curry
      osi = uncurry
     
instance Iso (a->b->c) (b->a->c) where
     iso = flip
     osi = flip

instance (Iso a b) => Iso [a] [b] where
     iso = map iso
     osi = map osi
     
instance (Functor f, Iso a b) => Iso (f a) (f b) where     
     iso = fmap iso
     osi = fmap osi
  