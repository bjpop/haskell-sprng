module Main where

import Sprng
import Control.Monad (replicateM_)

main = do
   let seed = 42
   gen1 <- new seed
   [gen2, gen3, gen4] <- spawnRng gen1 3
   printRandInts gen1 10
   printRandDoubles gen1 10
   printRandInts gen2 10
   printRandDoubles gen2 10
   printRandInts gen3 10
   printRandDoubles gen3 10
   printRandInts gen4 10
   printRandDoubles gen4 10

printRandInts :: RNG -> Int -> IO ()
printRandInts rng num =
   replicateM_ num (print =<< randomInt rng)

printRandDoubles :: RNG -> Int -> IO ()
printRandDoubles rng num =
   replicateM_ num (print =<< randomDouble rng)
