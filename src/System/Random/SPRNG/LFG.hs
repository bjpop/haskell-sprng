-- | This library implements 
--
-- It provides several types of generator algorithms:
--
--    * Additive Lagged Fibonacci (LFG)
--
-- The generators are splittable, which means that new generators can
-- be spawned from existing ones (many times, though not infinitely).

module System.Random.SPRNG.LFG
   ( RNG {- abstract -}
   , new
   , randomInt
   , randomFloat
   , randomDouble
   , spawn
   -- , mkNewRng
   ) where

import qualified System.Random.SPRNG.LFG.Internal as Internal

-- | An abstract Random Number Generator
data RNG = RNG Internal.LFG

-- | Construct a new generator, given a seed as input, and initialise with default parameters.
new :: Int -- ^ Seed.
    -> IO RNG -- ^ Initialised generator.
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

-- default implementations which just call the underlying SPRNG libs.

