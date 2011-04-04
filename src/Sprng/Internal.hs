{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Sprng.Internal
   ( LFGPtr
   , new_lfg
   , init_lfg
   , get_rn_int_lfg
   , get_rn_flt_lfg
   , get_rn_dbl_lfg
   , free_lfg
   ) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt, CFloat, CDouble)

type LFGPtr = Ptr ()

foreign import ccall "new_lfg" new_lfg :: IO LFGPtr
foreign import ccall "init_lfg" init_lfg :: LFGPtr -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "get_rn_int_lfg" get_rn_int_lfg :: LFGPtr -> IO CInt
foreign import ccall "get_rn_flt_lfg" get_rn_flt_lfg :: LFGPtr -> IO CFloat
foreign import ccall "get_rn_dbl_lfg" get_rn_dbl_lfg :: LFGPtr -> IO CDouble
foreign import ccall "free_lfg" free_lfg :: LFGPtr -> IO ()
