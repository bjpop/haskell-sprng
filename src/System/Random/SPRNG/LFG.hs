{-# LANGUAGE DeriveDataTypeable, CPP, TypeFamilies, FlexibleContexts #-}

-- | This library implements an Modified Additive Lagged Fibonacci
-- Pseudo Random Number Generator (LFG). It is based on an FFI binding
-- to the Scalable Parallel Pseudo Random Number Generator library
-- (SPRNG) version 4.
-- It provides a good quality fast splittable (aka spawnable) generator.
--
-- Generators are represented by the abstract type 'Gen'.
--
-- New generators are constructed using the 'create' function, which takes
-- a seed as its argument:
--
-- @
--    let seed = 42
--    gen <- create seed
-- @
--
-- Given a generator, random values can be generated using the overloaded
-- function 'uniform':
--
-- @
--    nextInt    <- uniform gen :: IO Int
--    nextDouble <- uniform gen :: IO Double
-- @
--
-- A random value from a given range can be generated using the overloaded
-- function 'uniformR':
--
-- @
--    nextInt    <- uniformR (1, 10) gen :: IO Int
--    nextDouble <- uniformR (0.5, 3.2) gen :: IO Double
-- @
--
-- A list of new generators can be spawned from an existing generator
-- using the 'spawn' function, by specifiying how many generators you want:
--
-- @
--    newGens <- spawn gen 12
-- @
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

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module System.Random.SPRNG.LFG
   ( Gen
   , create
   , randomWords -- this is useful for generating instances of Variate for other types
   , spawn
   , Variate (..)
   ) where

import Control.Monad (ap, liftM)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.C.Types (CUInt)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Bits ((.&.), (.|.), shiftL)
import Control.Applicative ((<$>))
import qualified System.Random.SPRNG.LFG.Internal as Internal

-- | An abstract Random Number Generator
data Gen = Gen Internal.LFG

-- | Construct a new generator, given a seed as input.
create :: Int -- ^ Seed.
    -> IO Gen -- ^ New generator.
create seed = do
   lfg <- Internal.new
   -- Note: we use default parameters to initialise the generator.
   -- This uses lags of (1279,861) which are the largest provided
   -- by the SPRNG library. Larger lags give generators with longer
   -- periods, but use more memory.
   Internal.initialise lfg 0 1 seed 0
   return $ Gen lfg

{-
-- | Initialise a previously uninitialised generator.
initRng :: Gen   -- ^ Uninitialised generator.
        -> Int   -- ^ Stream number.
        -> Int   -- ^ Number of streams.
        -> Int   -- ^ Seed.
        -> Int   -- ^ Gen specific parameter.
        -> IO () -- ^ Initialised generator.
initRng (Gen lfg) streamnum nstreams seed param =
   Internal.initRng lfg streamnum nstreams seed param
-}

-- | Generate a random (32 bit) Int.
randomInt :: Gen -> IO CUInt
randomInt (Gen lfg) = Internal.getRandomInt lfg

-- | Generate a random Float.
randomFloat :: Gen -> IO Float
randomFloat (Gen lfg) = Internal.getRandomFloat lfg

-- | Generate a random Double.
randomDouble :: Gen -> IO Double
randomDouble (Gen lfg) = Internal.getRandomDouble lfg

-- | Generate two random 32 bit Words.
randomWords :: Gen -> IO (Word32, Word32)
randomWords (Gen lfg) = Internal.getRandomWords lfg

-- | Create new generators from an existing one.
spawn :: Gen -> Int -> IO [Gen]
spawn (Gen lfg) n = map Gen `fmap` Internal.spawnRng lfg n

{-
printRng :: Gen a -> IO ()
printRng (Gen rng) = Internal.printRng rng
-}

-- From here on down, the rest of the code is taken directly from the random-mwc
-- library. Thanks to Bryan O'Sullivan for writing the code. The code is
-- copyright 2009, 2010, 2011 to Bryan O'Sullivan, and released under the BSD3
-- license.

-- | The class of types for which we can generate uniformly
-- distributed random variates.
class Variate a where
    -- | Generate a single uniformly distributed random variate.  The
    -- range of values produced varies by type:
    --
    -- * For fixed-width integral types, the type's entire range is
    --   used.
    --
    -- * For floating point numbers, the range (0,1] is used. Zero is
    --   explicitly excluded, to allow variates to be used in
    --   statistical calculations that require non-zero values
    --   (e.g. uses of the 'log' function).
    --
    -- To generate a 'Float' variate with a range of [0,1), subtract
    -- 2**(-33).  To do the same with 'Double' variates, subtract
    -- 2**(-53).
    uniform :: Gen -> IO a
    -- | Generate single uniformly distributed random variable in a
    -- given range.
    --
    -- * For integral types inclusive range is used.
    --
    -- * For floating point numbers range (a,b] is used if one ignores
    --   rounding errors.
    uniformR :: (a, a) -> Gen -> IO a

uniform1 :: (Word32 -> a) -> Gen -> IO a
uniform1 f gen = do
  (w1, _w2) <- randomWords gen
  return $! f w1
{-# INLINE uniform1 #-}

-- | Yield a random value, by transforming two sequential Word32 values.
uniform2 :: (Word32 -> Word32 -> a) -> Gen -> IO a
uniform2 f gen = do
   (w1, w2) <- randomWords gen
   return $! f w1 w2
{-# INLINE uniform2 #-}

instance Variate Int8 where
    uniform gen = fromIntegral <$> randomInt gen
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int16 where
    uniform gen = fromIntegral <$> randomInt gen
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int32 where
    uniform gen = fromIntegral <$> randomInt gen
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int64 where
    uniform = uniform2 wordsTo64Bit
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word8 where
    uniform gen = fromIntegral <$> randomInt gen
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word16 where
    uniform gen = fromIntegral <$> randomInt gen
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word32 where
    uniform gen = fromIntegral <$> randomInt gen
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word64 where
    uniform = uniform2 wordsTo64Bit
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Bool where
    uniform = uniform1 wordToBool
    uniformR (False,True)  g = uniform g
    uniformR (False,False) _ = return False
    uniformR (True,True)   _ = return True
    uniformR (True,False)  g = uniform g
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Float where
    uniform = randomFloat
    uniformR (x1,x2) = uniform1 (\w -> x1 + (x2-x1) * wordToFloat w)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Double where
    uniform = randomDouble
    uniformR (x1,x2) = uniform2 (\w1 w2 -> x1 + (x2-x1) * wordsToDouble w1 w2)
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
    uniform gen = fromIntegral <$> randomInt gen
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
    uniform gen = fromIntegral <$> randomInt gen
#else
    uniform = uniform2 wordsTo64Bit
#endif
    uniformR = uniformRange
    {-# INLINE uniform #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b) => Variate (a,b) where
    uniform g = (,) `liftM` uniform g `ap` uniform g
    uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
    uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
    uniformR ((x1,y1,z1),(x2,y2,z2)) g =
      (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
    uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g
                `ap` uniform g
    uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
      (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                    uniformR (z1,z2) g `ap` uniformR (t1,t2) g
    {-# INLINE uniform  #-}
    {-# INLINE uniformR #-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

-- Type family for fixed size integrals. For signed data types it's
-- its unsigned couterpart with same size and for unsigned data types
-- it's same type
type family Unsigned a :: *

type instance Unsigned Int8  = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64
type instance Unsigned Int   = Word

type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64
type instance Unsigned Word   = Word

-- Subtract two numbers under assumption that x>=y and store result in
-- unsigned data type of same size
sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x

-- Generate uniform value in the range [0,n). Values must be
-- unsigned. Second parameter is random number generator
unsignedRange :: (Integral a, Bounded a) => a -> IO a -> IO a
unsignedRange n rnd = go
  where
    buckets = maxBound `div` n
    maxN    = buckets * n
    go = do x <- rnd
            if x < maxN then return (x `div` buckets)
                        else go
{-# INLINE unsignedRange #-}

-- Generate unformly distributed value in inclusive range.
uniformRange :: (Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => (a,a) -> Gen -> IO a
uniformRange (x1,x2) g
  | x1 == minBound && x2 == maxBound = uniform g
  | otherwise                        = do x <- unsignedRange (sub x2 x1 + 1) (uniform g)
                                          return $! add x1 x
{-# INLINE uniformRange #-}
