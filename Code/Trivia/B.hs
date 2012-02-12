module B where
data B = F | T

class E a where
  eq :: a -> a -> a
instance E B where 
  eq x y = case x of
    T -> y
    F -> case y of 
      T -> F
      F -> T
