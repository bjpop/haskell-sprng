{-# LANGUAGE EmptyDataDecls #-}

module Sprng
   ( RNG {- abstract -}
   , SprngGen (..)
   , LFG, LCG, LCG64, CMRG, MLFG, PMLCG
   , Internal.newSeed
   ) where

import qualified Sprng.Internal as Internal

data RNG a = RNG Internal.Sprng

data LFG
data LCG
data LCG64
data CMRG
data MLFG
data PMLCG

class SprngGen a where
   newRng :: IO (RNG a)
   initRng :: RNG a -> Int -> Int -> Int -> Int -> IO ()
   randomInt :: RNG a -> IO Int
   randomFloat :: RNG a -> IO Float
   randomDouble :: RNG a -> IO Double
   spawnRng :: RNG a -> Int -> IO [RNG a]
   printRng :: RNG a -> IO ()
   printRng (RNG rng) = Internal.printRng rng

   randomInt (RNG rng) = Internal.getRandomInt rng
   randomFloat (RNG rng) = Internal.getRandomFloat rng
   randomDouble (RNG rng) = Internal.getRandomDouble rng
   spawnRng (RNG rng) n = map RNG `fmap` Internal.spawnRng rng n
   initRng (RNG rng) = Internal.initRng rng

instance SprngGen LFG where
   newRng = RNG `fmap` Internal.newRng Internal.LFG

instance SprngGen LCG where
   newRng = RNG `fmap` Internal.newRng Internal.LCG

instance SprngGen LCG64 where
   newRng = RNG `fmap` Internal.newRng Internal.LCG64

instance SprngGen CMRG where
   newRng = RNG `fmap` Internal.newRng Internal.CMRG

instance SprngGen MLFG where
   newRng = RNG `fmap` Internal.newRng Internal.MLFG

instance SprngGen PMLCG where
   newRng = RNG `fmap` Internal.newRng Internal.PMLCG
