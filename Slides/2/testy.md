# Testowanie programów w Haskellu
* HUnit
* Quickcheck

# HUnit

W większości języków powszechną praktyką jest stosowanie testów jednostkowych.

Mozna to robić i w Haskellu., np.

~~~~ {.haskell}
import Test.HUnit
import MyArray

main = runTestTT tests

tests = TestList [test1,test2]

listArray1 es = listArray (1,length es) es
test1 = TestCase$assertEqual "a!2 = 2" (listArray1 [1..3] ! 2) 2
test2 = TestCase$assertEqual "elems . array = id" 
                             (elems $ listArray1 [1..3]) [1..3]
~~~~

albo

~~~~ {.haskell}
import Test.HUnit

run = runTestTT tests
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)
~~~~

~~~~
*Main Test.HUnit> run
Cases: 2  Tried: 2  Errors: 0  Failures: 0
Counts {cases = 2, tried = 2, errors = 0, failures = 0}

*Main Test.HUnit> :t runTestTT
runTestTT :: Test -> IO Counts
~~~~

# Posortujmy listę

~~~~ {.haskell}
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort pred = go
  where
    go []  = []
    go [x] = [x]
    go xs  = merge (go xs1) (go xs2)
      where (xs1,xs2) = split xs

    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | pred x y  = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
~~~~ {.haskell}


# Funkcja split

...tworzy dwie podlisty podobnej długości, które będzie można po posortowaniu złączyć

~~~~ {.haskell}
split :: [a] -> ([a],[a])
split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:xs,y:ys)
  where (xs,ys) = split zs
~~~~ {.haskell}


# Sortowanie: testy jednostkowe


~~~~
sort = mergeSort ((<=) :: Int -> Int -> Bool)

sort [1,2,3,4] == [1,2,3,4]
sort [4,3,2,1] == [1,2,3,4]
sort [1,4,2,3] == [1,2,3,4]
...
~~~~

To się robi trochę nudne...

...ale dzięki typom, można lepiej.

# Własności

oczywista własność sortowania:

~~~~ {.haskell}
prop_idempotent = sort . sort == sort
~~~~

nie jest definiowalna; nie możemy porównywać funkcji.

Możemy "oszukać":

~~~~ {.haskell}
prop_idempotent xs = 
    sort (sort xs) == sort xs
~~~~

Spróbujmy w interpreterze:

~~~~
*Main> prop_idempotent [3,2,1]
True
~~~~

# Próba mechanizacji

Możemy to próbować zmechanizować:

~~~~
*Main> prop_permute prop_idempotent [1,2,3]
True
*Main> prop_permute prop_idempotent [1..4]
True
*Main> prop_permute prop_idempotent [1..5]
True
*Main> prop_permute prop_idempotent [1..10]
  C-c C-cInterrupted.
~~~~

# QuickCheck

* Generowanie dużej ilości testów jednostkowych jest żmudne

* Sprawdzenie wszystkich możliwości jest nierealistyczne

* Pomysł: wygenerować odpowiednią losową próbkę danych

~~~~
*Main> import Test.QuickCheck
*Main Test.QuickCheck> quickCheck prop_idempotent
+++ OK, passed 100 tests.
~~~~

QuickCheck wylosował 100 list i sprawdził własność,

Możemy zażyczyć sobie np. 1000:

~~~~
*Main Test.QuickCheck> quickCheckWith stdArgs {maxSuccess = 1000}  prop_idempotent
+++ OK, passed 1000 tests.
~~~~

# Jak to działa?

Dla uproszczenia najpierw przyjrzyjmy się starszej wersji

Główne składniki

~~~~ {.haskell}

quickCheck  :: Testable a => a -> IO ()
quickCheck   = check quick


check :: Testable a => Config -> a -> IO ()
quick :: Config

class Testable a where
  property :: a -> Property

instance Testable Bool where...

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a -> Gen b -> Gen b
~~~~

# Generacja liczb losowych

~~~~ {.haskell}
import System.Random
  ( StdGen       -- :: *
  , newStdGen    -- :: IO StdGen
  , randomR      -- :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  , split        -- :: RandomGen g => g -> (g, g)
                 -- rozdziela argument na dwa niezależne generatory
  -- instance RandomGen StdGen
  -- instance Random Int  
  )
  
roll :: StdGen -> Int
roll rnd = fst $ randomR (1,6) rnd
main = do 
  rnd <- newStdGen 
  let (r1,r2) = split rnd
  print (roll r1)
  print (roll r2)
  print (roll r1)
  print (roll r2)
~~~~

~~~~
*Main System.Random> main
4
5
4
5
~~~~

# Generatory losowych obiektów

~~~~ {.haskell}
-- generator bierze pożądany rozmiar i StdGen i daje a
newtype Gen a
  = Gen (Int -> StdGen -> a)

chooseInt1 :: (Int,Int) -> Gen Int
chooseInt1 bounds = Gen $ \n r  -> fst (randomR bounds r)

rand :: Gen StdGen
rand = Gen (\n r -> r)

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

resize :: Int -> Gen a -> Gen a
resize n (Gen m) = Gen (\_ r -> m n r)
~~~~

# Monada generatorów

~~~~ {.haskell}
-- Trochę jak monada stanu, tylko musimy rozdzielić "stan" na dwa
instance Monad Gen where
  return a = Gen $ \n r -> a
  Gen m >>= k = Gen $ \n r0 ->
    let (r1,r2) = split r0
        Gen m'  = k (m n r1)
     in m' n r2

instance Functor Gen where
  fmap f m = m >>= return . f
                     
chooseInt :: (Int,Int) -> Gen Int
chooseInt bounds = (fst . randomR bounds) `fmap` rand

choose ::  Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand
~~~~

# Arbitrary

~~~~ {.haskell}
class Arbitrary a where
  arbitrary   :: Gen a

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | i <- [1..n] ]
-- sequence :: Monad m => [m a] -> m [a]
instance Arbitrary () where
  arbitrary = return ()
  
instance Arbitrary Bool where
  arbitrary     = elements [True, False]
  
instance Arbitrary a => Arbitrary [a] where
  arbitrary          = sized (\n -> choose (0,n) >>= vector)

instance Arbitrary Int where
  arbitrary     = sized $ \n -> choose (-n,n)
~~~~

# forAll

~~~~ {.haskell}
evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen = property a 
                       
forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do a   <- gen
     res <- evaluate (body a)
     return (argument a res)
 where
  argument a res = res{ arguments = show a : arguments res }


propAddCom1 :: Property
propAddCom1 =  forAll (chooseInt (-100,100)) (\x -> x + 1 == 1 + x)
propAddCom2 =  forAll int (\x -> forAll int (\y -> x + y == y + x)) where
  int = chooseInt (-100,100)
~~~~

~~~~
>>> check $ forAll (chooseInt (-100,100)) (\x -> x + 0 == x)
OK, passed 100 tests
>>> check $ forAll (chooseInt (-100,100)) (\x -> x + 1 == x)
Falsifiable, after 0 tests:
-22
~~~~

# Funkcje i implikacja

Mając forAll, funkcje są zaskakująco łatwe:

~~~~ {.haskell}
instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

propAddCom3 :: Int -> Int -> Bool  
propAddCom3 x y = x + y == y + x
~~~~

Jeszcze implikacja: jeśli p to q

~~~~ {.haskell}
(==>) :: Testable a => Bool -> a -> Property
True  ==> a = property a
False ==> a = property ()

propMul1 :: Int -> Property
propMul1 x = (x>0) ==> (2*x > 0) 

propMul2 :: Int -> Int -> Property
propMul2 x y = (x>0) ==> (x*y > 0) 
~~~~

~~~~
> check propMul1
OK, passed 100 tests

> check propMul2
Falsifiable, after 0 tests:
2
-2
~~~~


