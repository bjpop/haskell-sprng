{-# LANGUAGE EmptyDataDecls #-}
-- | This library implements a Haskell interface to the
-- Scalable Parallel Pseudo Random Number Generators Library (SPRNG)
-- <http://sprng.cs.fsu.edu>.
--
-- It provides several types of generator algorithms:
--
--    * Additive Lagged Fibonacci (LFG)
--
--    * Multiplicative Lagged Fibonacci (MLFG)
--
--    * Linear Congruential (LCG)
--
--    * Combined Multiple Recursive (CMRG)
--
--    * Prime Modulus Linear Congruential (PMLCG)
--
-- The generators are splittable, which means that new generators can
-- be spawned from existing ones (many times, though not infinitely).

module Sprng
   ( RNG {- abstract -}
   , UnInitRNG {- abstract -}
   , SprngGen (..)
   , LFG, LCG, LCG64, CMRG, MLFG, PMLCG
   , Internal.newSeed
   ) where

import qualified Sprng.Internal as Internal

-- | An abstract Random Number Generator, parameterised by the type of algorithm used.
data RNG a = RNG Internal.Sprng

-- | An uninitialised abstract Random Number Generator, parameterised by the type of algorithm used.
data UnInitRNG a = UnInitRNG Internal.Sprng

-- | A token representing the Additive Lagged Fibonacci Generator.
data LFG
-- | A token representing the Linear Congruential Generator.
data LCG
-- | A token representing the 64bit Linear Congruential Generator.
data LCG64
-- | A token representing the Combined Multiple Recursive Generator.
data CMRG
-- | A token representing the Multiplicative Lagged Fibonnaci Generator.
data MLFG
-- | A token representing the Prime Modulus Linear Congruential Generator.
data PMLCG

-- | Interface to the SPRNG generators
class SprngGen a where

   -- | Construct a new generator, given a seed as input, and initialise with default parameters.
   newRng :: Int -- ^ Seed.
          -> IO (RNG a) -- ^ Initialised generator.

   -- | Construct a new generator but do not initialise it.
   newRngUninitialised  :: IO (UnInitRNG a)

   -- | Initialise a previously uninitialised generator.
   initRng :: UnInitRNG a -- ^ Uninitialised generator.
           -> Int         -- ^ Stream number.
           -> Int         -- ^ Number of streams.
           -> Int         -- ^ Seed.
           -> Int         -- ^ RNG specific parameter.
           -> IO (RNG a)  -- ^ Initialised generator.

   -- | Generate a random Int.
   randomInt :: RNG a -> IO Int

   -- | Generate a random Float.
   randomFloat :: RNG a -> IO Float

   -- | Generate a random Double.
   randomDouble :: RNG a -> IO Double

   -- | Create new generators from an existing one.
   spawnRng :: RNG a -> Int -> IO [RNG a]

   -- | Print a representation of the generator for diagnostic purposes.
   printRng :: RNG a -> IO ()
   printRng (RNG rng) = Internal.printRng rng

   -- default implementations which just call the underlying SPRNG libs.
   randomInt (RNG rng) = Internal.getRandomInt rng
   randomFloat (RNG rng) = Internal.getRandomFloat rng
   randomDouble (RNG rng) = Internal.getRandomDouble rng
   spawnRng (RNG rng) n = map RNG `fmap` Internal.spawnRng rng n
   initRng (UnInitRNG rng) streamnum nstreams seed param = do
       Internal.initRng rng streamnum nstreams seed param
       return $ RNG rng

-- construct a new RNG and initialise it.
mkNewRng :: Internal.RngType -> Int -> IO (RNG a)
mkNewRng rngType seed = do
      rng <- Internal.newRng rngType
      defaultParam <- Internal.sprngDefault
      Internal.initRng rng 0 1 seed defaultParam
      return $ RNG rng

instance SprngGen LFG where
   newRng = mkNewRng Internal.LFG
   newRngUninitialised = UnInitRNG `fmap` Internal.newRng Internal.LFG

instance SprngGen LCG where
   newRng = mkNewRng Internal.LCG
   newRngUninitialised = UnInitRNG `fmap` Internal.newRng Internal.LCG

instance SprngGen LCG64 where
   newRng = mkNewRng Internal.LCG64
   newRngUninitialised = UnInitRNG `fmap` Internal.newRng Internal.LCG64

instance SprngGen CMRG where
   newRng = mkNewRng Internal.CMRG
   newRngUninitialised = UnInitRNG `fmap` Internal.newRng Internal.CMRG

instance SprngGen MLFG where
   newRng = mkNewRng Internal.MLFG
   newRngUninitialised = UnInitRNG `fmap` Internal.newRng Internal.MLFG

instance SprngGen PMLCG where
   newRng = mkNewRng Internal.PMLCG
   newRngUninitialised = UnInitRNG `fmap` Internal.newRng Internal.PMLCG
