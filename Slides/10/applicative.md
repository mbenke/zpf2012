# Motywacja - sekwencje

Przypomnijmy sobie parser dla cyfr:

~~~~ {.haskell}
pNum :: Parser Int
pNum = fmap digitToInt digit
~~~~

co jest krótszym zapisem

~~~~ {.haskell}
pNum = do
     d <- digit
     return $ digitToInt d
~~~~

# Motywacja - sekwencje

Przypomnijmy sobie funkcję `sequence` (tu dla monady IO):

~~~~ {.haskell}
sequence1 :: [IO a] -> IO [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)
~~~~

która "wykonuje" listę akcji zbierając ich wyniki w liste.

Możemy ją zapisac prościej uzywając "monadycznej" aplikacji, `ap`:

~~~~ {.haskell}
sequence2 (c:cs) = return (:) `ap` c `ap` cs

ap mf mx = do
  f <- mf
  x <- mx
  return $ f x
~~~~

# Motywacja - `zipWith`

~~~~ {.haskell}
zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith_n :: (a1 -> ... -> an -> b) -> [a1] -> ... -> [an] -> [b]
zipWith_n f as1 ... asn = repeat f `zap` as1 `zap` ... `zap` asn

zap :: [a->b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x:zap fs xs
zap _ _ = []
~~~~
