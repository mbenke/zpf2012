import Criterion.Main

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


main = defaultMain [
         bench "fib 10" $ whnf fib 10
       , bench "fib 30" $ whnf fib 20
       , bench "fib 35" $ whnf fib 35
       ]
