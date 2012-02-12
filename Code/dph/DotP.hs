{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}

module DotP where
import qualified Prelude
import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double as D

dotp_double :: [:Double:] -> [:Double:] -> Double
dotp_double xs ys = D.sumP [:x * y | x <- xs | y <- ys:]
 
dotp_wrapper :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp_wrapper #-}
dotp_wrapper v w = dotp_double (fromPArrayP v) (fromPArrayP w)