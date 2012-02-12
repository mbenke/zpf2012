{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}  
{-# LANGUAGE UndecidableInstances #-}

module Gadt where
import Data.Monoid

data Foo a where
  Bar :: Foo a
  Baz :: a -> Foo a -> Foo a

data Moo a where
  Mud :: Moo ()
  Mar :: a -> Moo a -> Moo Int

data Exp = ALitInt Int | ALitStr String | APlus Exp Exp

-- APlus (ALitStr "foo") (ALitInt 42)

data Expr a where
  ELitInt :: Int -> Expr Int
  ELitStr :: String -> Expr String
  EPlus :: Expr a -> Expr a -> Expr a
  
deriving instance Show a => Show(Expr a)  
deriving instance Eq a => Eq(Expr a)  

instance IsString (Expr String) where
  fromString = ELitStr
  
class HasPlus a where
  plus :: a -> a -> a 
  
-- instance (Num a) => HasPlus a where
-- plus = (+)
  
instance Monoid a => HasPlus a where
  plus = mappend

instance Monoid Int where
  mempty = 0
  mappend = (+)
interpret :: HasPlus a => Expr a -> a
interpret (ELitInt i) = i
interpret (ELitStr s) = s
interpret (EPlus e1 e2) = plus (interpret e1) (interpret e2)