{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sprng.Internal
   ( Sprng
   , RngType (..)
   , newRng
   , initRng
   , getRandomInt
   , getRandomFloat
   , getRandomDouble
   , printRng
   , spawnRng
   , newSeed
   ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, ForeignPtr)
import Foreign.C.Types (CInt, CFloat, CDouble)
import Foreign.Marshal.Array (peekArray)

type Sprng = ForeignPtr ()
type SprngPtr = Ptr ()
data RngType = LFG | LCG | LCG64 | CMRG | MLFG | PMLCG
   deriving (Eq, Show, Enum)

foreign import ccall "new_seed" newSeed :: IO Int
foreign import ccall "new_rng" new_rng :: CInt -> IO SprngPtr
foreign import ccall "init_rng" init_rng :: SprngPtr -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "get_rn_int" get_rn_int :: SprngPtr -> IO CInt
foreign import ccall "get_rn_flt" get_rn_flt :: SprngPtr -> IO CFloat
foreign import ccall "get_rn_dbl" get_rn_dbl :: SprngPtr -> IO CDouble
foreign import ccall "print_rng" print_rng :: SprngPtr -> IO ()
foreign import ccall "spawn_rng" spawn_rng :: SprngPtr -> CInt -> IO (Ptr SprngPtr)
foreign import ccall "&free_rng" freeRngFunPtr :: FunPtr (SprngPtr -> IO ())
foreign import ccall "free_spawn_buffer" freeSpawnBuffer :: Ptr (SprngPtr) -> IO ()

newRng :: RngType -> IO Sprng
newRng ty = do
   ptr <- new_rng $ fromIntegral $ fromEnum ty
   newForeignPtr freeRngFunPtr ptr

initRng :: Sprng -> Int -> Int -> Int -> Int -> IO ()
initRng rng streamnum nstreams seed param =
   withForeignPtr rng $ \ptr ->
      init_rng ptr (fromIntegral streamnum)
                   (fromIntegral nstreams)
                   (fromIntegral seed)
                   (fromIntegral param)

-- Haskell Int is machine word sized, which is safe to fit a C int.
getRandomInt :: Sprng -> IO Int
getRandomInt rng =
   withForeignPtr rng $ \ptr -> fromIntegral `fmap` get_rn_int ptr

getRandomFloat :: Sprng -> IO Float
getRandomFloat rng =
   withForeignPtr rng $ \ptr -> realToFrac `fmap` get_rn_flt ptr

getRandomDouble :: Sprng -> IO Double
getRandomDouble rng =
   withForeignPtr rng $ \ptr -> realToFrac `fmap` get_rn_dbl ptr

printRng :: Sprng -> IO ()
printRng rng = withForeignPtr rng $ print_rng

spawnRng :: Sprng -> Int -> IO [Sprng]
spawnRng rng num =
   withForeignPtr rng $ \ptr -> do
      arr <- spawn_rng ptr $ fromIntegral num
      ptrs <- peekArray num arr
      rngs <- mapM (newForeignPtr freeRngFunPtr) ptrs
      freeSpawnBuffer arr
      return rngs
