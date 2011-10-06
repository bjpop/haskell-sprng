module Main where

import Prelude hiding (init)
import System.Random.SPRNG.LFG (RNG, new, randomInt, randomDouble, spawn)
import Control.Monad (replicateM_)
import Data.Int (Int32)
import Text.Printf

main = do
   let seed = 42
       depth = 6
   gen <- new seed
   spawnTest gen depth

spawnTest :: RNG -> Int -> IO ()
spawnTest gen depth
    | depth <= 0 = return ()
    | otherwise = do
         replicateM_ 100 (print =<< (randomInt gen :: IO Int))
         replicateM_ 100 (printDouble =<< (randomDouble gen :: IO Double))
         [gen1,gen2] <- spawn gen 2
         let newDepth = depth - 1
         spawnTest gen1 newDepth
         spawnTest gen2 newDepth

printDouble :: Double -> IO ()
printDouble  = printf "%.20f\n"
