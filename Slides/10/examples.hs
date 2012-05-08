-- import Prelude hiding(sequence)
import Data.Maybe(fromJust)

sequence1 :: [IO a] -> IO [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)

sequence2 :: [IO a] -> IO [a]
sequence2 (c:cs) = return (:) `ap` c `ap` sequence cs

ap mf mx = do
  f <- mf
  x <- mx
  return $ f x


zap :: [a->b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x:zap fs xs
zap _ _ = []


transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = repeat (:) `zap` xs `zap` transpose xss
-- zipWith (:) xs (transpose xss)

data Exp v = Var v
     	   | Val Int
           | Add (Exp v) (Exp v)

type Env v = [(v,Int)]
fetch :: v -> Env v -> Int
fetch v e = undefined

eval1 :: Exp v -> Env v -> Int
eval1 (Var x) env = fetch x env
eval1 (Val i) env = i
eval1 (Add p q) env = eval1 p env + eval1 q env

eval2 (Var x) = fetch x
eval2 (Val i) = k i
eval2 (Add p q) = k (+) `s` eval2 p `s` eval2 q
k x y = x
s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es env = (ef env) (es env)