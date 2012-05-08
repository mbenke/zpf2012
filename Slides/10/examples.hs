-- import Prelude hiding(sequence)

sequence1 :: [IO a] -> IO [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)

zap :: [a->b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x:zap fs xs
zap _ _ = []
