# Równoległość a współbieżność

A *parallel* program is one that uses a multiplicity of computational
hardware (e.g. multiple processor cores) in order to perform
computation more quickly.  Different parts of the computation are
delegated to different processors that execute at the same time (in
parallel), so that results may be delivered earlier than if the
computation had been performed sequentially.

In contrast, *concurrency* is a program-structuring technique in which
there are multiple threads of control. Notionally the threads of
control execute "at the same time"; that is, the user sees their
effects interleaved. Whether they actually execute at the same time or
not is an implementation detail; a concurrent program can execute on a
single processor through interleaved execution, or on multiple
physical processors. 

--- Simon Marlow, *Parallel and Concurrent Programming in Haskell*.

# Równoległość w Haskellu

Tu zajmujemy się głównie równoległościę, czyli wykorzystaniem wielu rdzeni/procesorów dla przyśpieszenia obliczeń.

# Sudoku

Przykład z dużą ilościa obliczeń: rozwiązywanie Sudoku

Każda linia pliku wejściowego zawiera instancję problemu.

Program sekwencyjny:

~~~~ {.haskell}
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    mapM_ (evaluate . solve) grids
~~~~

~~~~
$ ./sudoku1 sudoku17.1000.txt +RTS -s
  Task  0 (worker) :    0.00s    (  0.00s)       0.00s    (  0.00s)
  Task  1 (worker) :    0.00s    (  2.37s)       0.00s    (  0.00s)
  Task  2 (bound)  :    2.32s    (  2.32s)       0.05s    (  0.05s)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    2.32s  (  2.32s elapsed)
  GC      time    0.05s  (  0.05s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    2.37s  (  2.37s elapsed)
~~~~

# Program równoległy

~~~~ {.haskell}
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    let (as,bs) = splitAt (length grids `div` 2) grids

    evaluate $ runEval $ do
       a <- rpar (deep (map solve as))
       b <- rpar (deep (map solve bs))
       rseq a
       rseq b
       return ()
~~~~

# Koniec

~~~~ {.haskell}

~~~~
