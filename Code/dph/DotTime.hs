import System.CPUTime (getCPUTime)
import System.Random (newStdGen)
import Control.Exception (evaluate)
import Data.Array.Parallel.PArray (PArray, randomRs, nf)
 
import DotP (dotp_wrapper)  -- import vectorised code
 
main :: IO ()
main
  = do 
      -- generate random input vectors
      gen1 <- newStdGen
      gen2 <- newStdGen
      let v = randomRs n range gen1
          w = randomRs n range gen2
 
      -- force the evaluation of the input vectors
      evaluate $ nf v
      evaluate $ nf w
 
      -- timed computations
      start <- getCPUTime
      let result = dotp_wrapper v w
      evaluate result
      end <- getCPUTime
 
      -- print the result
      putStrLn $ show result ++ " in " ++ show ((end - start) `div` 1000000) ++ "us"
  where
    n     = 1000000        -- vector length
    range = (-100, 100)  -- range of vector elements
