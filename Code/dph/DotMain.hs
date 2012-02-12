import Data.Array.Parallel
import Data.Array.Parallel.PArray (PArray, fromList)
 
import DotP (dotp_wrapper)  -- import vectorised code
 
main :: IO ()
main
  = let v      = fromList [1..10]    -- convert lists...
        w      = fromList [1,2..20]  -- ...to parallel arrays
        result = dotp_wrapper v w    -- invoke vectorised code
    in
    print result                     -- print the result
