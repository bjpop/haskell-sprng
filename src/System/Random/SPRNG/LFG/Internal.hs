{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module System.Random.SPRNG.LFG.Internal
   ( LFG
   , new
   , initialise
   , getRandomInt
   , getRandomFloat
   , getRandomDouble
   , getRandomWords
   , spawnRng
   ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, ForeignPtr)
import Foreign.C.Types (CUInt(..), CInt(..), CFloat(..), CDouble(..))
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Data.Word (Word32)

type LFG = ForeignPtr ()
type LFGPtr = Ptr ()

-- the calls are marked as "unsafe" because they do not call back into
-- the ghc runtime and should not block for a long time. The "unsafe"
-- attribute allows much faster foreign calls to be made, and this provides
-- a signficant performance benefit, especially for the functions which are
-- typically called frequently (such as get_rn_int).

-- | Construct a new RNG.
foreign import ccall unsafe "new_rng" new_rng :: IO LFGPtr

-- | Initialise a new RNG.
foreign import ccall unsafe "init_rng" init_rng :: LFGPtr -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Generate a new random int.
foreign import ccall unsafe "get_rn_int" get_rn_int :: LFGPtr -> IO CUInt

-- | Generate a new random float.
foreign import ccall unsafe "get_rn_flt" get_rn_flt :: LFGPtr -> IO CFloat

-- | Generate a new random double.
foreign import ccall unsafe "get_rn_dbl" get_rn_dbl :: LFGPtr -> IO CDouble

-- | Generate a pair of random unsigned ints
foreign import ccall unsafe "get_rn_words" get_rn_words :: LFGPtr -> Ptr CUInt -> Ptr CUInt -> IO ()

-- | Print a RNG for diagnostic purposes.
-- foreign import ccall unsafe "print_rng" print_rng :: LFGPtr -> IO ()

-- | Spawn a new RNG from an existing one.
foreign import ccall unsafe "spawn_rng_wrapper" spawn_rng :: LFGPtr -> CInt -> IO (Ptr LFGPtr)

-- | Free the memory of an RNG.
foreign import ccall unsafe "&free_rng" freeRngFunPtr :: FunPtr (LFGPtr -> IO ())

-- | Free the memory allocated to a buffer of spawned RNGs.
foreign import ccall unsafe "free_spawn_buffer" freeSpawnBuffer :: Ptr (LFGPtr) -> IO ()

new :: IO LFG
new = newForeignPtr freeRngFunPtr =<< new_rng

initialise :: LFG -> Int -> Int -> Int -> Int -> IO ()
initialise lfg streamnum nstreams seed param =
   withForeignPtr lfg $ \ptr ->
      init_rng ptr (fromIntegral streamnum)
                   (fromIntegral nstreams)
                   (fromIntegral seed)
                   (fromIntegral param)

getRandomInt :: LFG -> IO CUInt
getRandomInt rng = withForeignPtr rng get_rn_int

getRandomFloat :: LFG -> IO Float
getRandomFloat rng =
   withForeignPtr rng $ \ptr -> realToFrac `fmap` get_rn_flt ptr

getRandomDouble :: LFG -> IO Double
getRandomDouble rng =
   withForeignPtr rng $ \ptr -> realToFrac `fmap` get_rn_dbl ptr

getRandomWords :: LFG -> IO (Word32, Word32)
getRandomWords rng =
   withForeignPtr rng $ \genPtr ->
      alloca $ \resultPtr1 ->
         alloca $ \resultPtr2 -> do
            get_rn_words genPtr resultPtr1 resultPtr2
            result1 <- peek resultPtr1
            result2 <- peek resultPtr2
            return (fromIntegral result1, fromIntegral result2)

{-
printRng :: LFG -> IO ()
printRng rng = withForeignPtr rng $ print_rng
-}

spawnRng :: LFG -> Int -> IO [LFG]
spawnRng rng num =
   withForeignPtr rng $ \ptr -> do
      arr <- spawn_rng ptr $ fromIntegral num
      ptrs <- peekArray num arr
      rngs <- mapM (newForeignPtr freeRngFunPtr) ptrs
      freeSpawnBuffer arr
      return rngs
