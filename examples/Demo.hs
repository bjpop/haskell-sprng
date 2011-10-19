module Main where

import System.Random.SPRNG.LFG
import Control.Monad (replicateM)

main = do
   let seed = 42
   -- create a new generator from a seed
   gen <- create seed
   -- print 10 random Ints from the generator
   print =<< (generateVals 10 gen :: IO [Int])
   -- print 10 random Doubles from the generator
   print =<< (generateVals 10 gen :: IO [Double])
   -- spawn three new generators from the initial one
   gens <- spawn gen 3
   -- print 10 random Ints from each of the new generators
   mapM_ (\g -> print =<< (generateVals 10 g :: IO [Int])) gens

-- generate a list of random values
generateVals :: Variate a => Int -> Gen -> IO [a]
generateVals num = replicateM num . uniform
