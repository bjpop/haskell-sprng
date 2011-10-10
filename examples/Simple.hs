module Main where

import System.Random.SPRNG.LFG
import Control.Monad (replicateM_)

main = do
   let seed = 42
   gen1 <- create seed
   [gen2, gen3, gen4] <- spawn gen1 3
   printRandInts gen1 10
   printRandDoubles gen1 10
   printRandInts gen2 10
   printRandDoubles gen2 10
   printRandInts gen3 10
   printRandDoubles gen3 10
   printRandInts gen4 10
   printRandDoubles gen4 10

printRandInts :: Gen -> Int -> IO ()
printRandInts rng num =
   replicateM_ num (print =<< (uniform rng :: IO Int))

printRandDoubles :: Gen -> Int -> IO ()
printRandDoubles rng num =
   replicateM_ num (print =<< (uniform rng :: IO Double))
