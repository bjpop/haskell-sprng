{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Sprng.Internal
   ( SprngPtr
   , new_rng
   , init_rng
   , get_rn_int
   , get_rn_flt
   , get_rn_dbl
   , free_rng
   , print_rng
   ) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt, CFloat, CDouble)

type SprngPtr = Ptr ()

foreign import ccall "new_rng" new_rng :: CInt -> IO SprngPtr
foreign import ccall "init_rng" init_rng :: SprngPtr -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "get_rn_int" get_rn_int :: SprngPtr -> IO CInt
foreign import ccall "get_rn_flt" get_rn_flt :: SprngPtr -> IO CFloat
foreign import ccall "get_rn_dbl" get_rn_dbl :: SprngPtr -> IO CDouble
foreign import ccall "free_rng" free_rng :: SprngPtr -> IO ()
foreign import ccall "print_rng" print_rng :: SprngPtr -> IO ()
