# Ćwiczenie - kontynuacje a stan

~~~~
Stan:
 s -> (a,s)

CPS:   s -> ((a,s) -> r) -> r
curry: s -> (a -> s -> r) -> r
flip:  (a -> s -> r) -> s -> r
~~~~

Zdefiniuj

~~~~ {.haskell}
type CS s a r = (a -> s -> r) -> s -> r
-- Odpowiednik Functor
csmap :: (a->b) -> CS s a r -> CS s b r

-- Odpowiednik Monad
cspure :: a -> CS s a r
csbind :: CS s a r -> (a -> CS s b r) -> CS s b r
~~~~

i sprawdź, że działa:

~~~~
>>> foo (\a s -> show a) 17
"42"
~~~~

# Ćwiczenie - kontynuacje a stan (2)

~~~~ {.haskell}
-- Odpowiednik MonadState
-- get :: MonadState s m => m s
csget :: CS s s r

-- put :: MonadState s m => s -> m ()
csput :: s -> CS s () r

csmodify :: (s->s) -> CS s () r
csmodify t = csget `csbind` (\s -> csput (t s))

cstick :: CS Int () r
cstick = csmodify (+1)

bar :: CS Int Int r
bar = csput 40 `csthen` cstick `csthen` cstick `csthen` csget
~~~~

...i sprawdź, że działa:

~~~~
*Main> bar const 0
42
~~~~

Uwaga:

* nie importuj Control.Monad.State
* nie zaglądaj do jego źródeł

# Ćwiczenie - kontynuacje a stan (3)

Zdefiniuj monadę stanu przy pomocy Cont:

~~~~ {.haskell}
{-# LANGUAGE TypeSynonymInstances,FlexibleInstances, MultiParamTypeClasses #-}
import Control.Monad.Cont
-- Uwaga: nie importujemy Control.Monad.State
class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do { s <- get; put (f s) }

-- w miejsce X wstaw wyrażenie używające Cont, s, r
type CSM s r a = X a 

instance MonadState s X where
...
-- Wskazówka: cont :: ((a->s->r)->s -> r) -> Cont (s->r) a
  
tick :: CSM Int r Int
tick = modify (+1)

baz :: CSM Int r Int
baz = do { put 40; tick; tick; get }
~~~~

...i sprawdź, że działa:

~~~~
*Main> runCont baz const 0
42
~~~~

# Ćwiczenie: kontynuacje a błędy

~~~~ {.haskell}
{-
Error: (a + e)
CPS: ((a + e) -> r) -> r
de Morgan: (a ->r,e -> r) -> r
curry: (a->r) -> (e->r) -> r
-}

type CE e a r = (e->r) -> (a->r) -> r
cemap :: (a->b) -> CE e a r -> CE e b r
cepure :: a -> CE e a r
cebind :: CE e a r -> (a -> CE e b r) -> CE e b r

throwCE :: e -> CE e a r
catchCE :: CE e a r -> (e -> CE e a r) -> CE e a r

uncurryCE :: ((e->r) -> (a->r) -> r) -> ((e ->r,a -> r) -> r)
-- Prelude.either :: (e->r) -> (a->r) -> Either e a ->r
-- ~ ((e->r), (a->r)) -> Either e a ->r
coeither :: (Either e a -> r) -> (e ->r, a -> r)
morgan1 :: ((e ->r,a -> r) -> r) -> (Either e a -> r) -> r
morgan2 :: ((Either e a -> r) -> r) -> (e -> r, a -> r) -> r

-- te funkcje ustanawiaja izomorfizm
iso1 :: ((e->r) -> (a->r) -> r) -> ((Either e a) -> r) ->r
iso2 :: ((Either e a -> r) -> r) -> (e -> r) -> (a -> r) -> r


newtype CEM e r a = CEM { runCEM :: Cont r (Either e a) }
toCEM :: CE e a r -> CEM e r a
fromCEM :: CEM e r a -> CE e a r

instance Monad (CEM e r) where ...  
instance (Error e) => MonadError e (CEM e r) where...
~~~~ 
