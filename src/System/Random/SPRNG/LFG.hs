-- | This library implements an Modified Additive Lagged Fibonacci
-- Pseudo Random Number Generator (LFG). It is based on an FFI binding
-- to the Scalable Parallel Pseudo Random Number Generator library
-- (SPRNG) version 4.
-- It provides a good quality fast splittable (aka spawnable) generator.
--
-- Generators are represented by the abstract type RNG.
--
-- New generators are constructed using the "new" function, which takes
-- a seed as its argument:
--
-- let seed = 42
-- gen <- new seed
--
-- Given a generator, random integers and doubles can be generated using
-- randomInt and randomDouble:
--
-- nextInt <- randomInt gen
-- nextDouble <- randomDouble gen
--
-- A list of new generators can be spawned from an existing generator
-- using the spawn function, by specifiying how many generators you want:
--
-- newGens <- spawn 12
--
-- Note: a large number of new independent generators can be spawned,
-- but the number is not infinite.
--
-- SPRNG is written in C++, but due to complications
-- with using C++ with the Haskell FFI (the need to use a C++ compiler
-- for linking to resolve mangled names), and also because of the
-- relative difficulty of building SPRNG from source, we have decided
-- to provide our own version of the LFG from SPRNG re-written in C.
-- The rewrite is fairly trivial because SPRNG does not really use C++
-- in any essential way, rather it is mostly a thin gloss over
-- ordinary C code.

module System.Random.SPRNG.LFG
   ( RNG
   , new
   , randomInt
   , randomFloat
   , randomDouble
   , spawn
   ) where

import qualified System.Random.SPRNG.LFG.Internal as Internal

-- | An abstract Random Number Generator
data RNG = RNG Internal.LFG

-- | Construct a new generator, given a seed as input, and initialise with default parameters.
new :: Int -- ^ Seed.
    -> IO RNG -- ^ New generator.
new seed = do
   lfg <- Internal.new
   Internal.initialise lfg 0 1 seed 0
   return $ RNG lfg

{-
-- | Initialise a previously uninitialised generator.
initRng :: RNG   -- ^ Uninitialised generator.
        -> Int   -- ^ Stream number.
        -> Int   -- ^ Number of streams.
        -> Int   -- ^ Seed.
        -> Int   -- ^ RNG specific parameter.
        -> IO () -- ^ Initialised generator.
initRng (RNG lfg) streamnum nstreams seed param =
   Internal.initRng lfg streamnum nstreams seed param
-}

-- | Generate a random Int.
randomInt :: RNG -> IO Int
randomInt (RNG lfg) = Internal.getRandomInt lfg

-- | Generate a random Float.
randomFloat :: RNG -> IO Float
randomFloat (RNG lfg) = Internal.getRandomFloat lfg

-- | Generate a random Double.
randomDouble :: RNG -> IO Double
randomDouble (RNG lfg) = Internal.getRandomDouble lfg

-- | Create new generators from an existing one.
spawn :: RNG -> Int -> IO [RNG]
spawn (RNG lfg) n = map RNG `fmap` Internal.spawnRng lfg n

{-
printRng :: RNG a -> IO ()
printRng (RNG rng) = Internal.printRng rng
-}
