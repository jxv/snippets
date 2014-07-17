{-# LANGUAGE ForeignFunctionInterface #-}

module Sin where

#include <math.h>

import Foreign.C

foreign import ccall unsafe "sin"
  c_sin :: CDouble -> CDouble

sin2 :: CDouble -> CDouble
sin2 n = (c_sin n) ^ 2

