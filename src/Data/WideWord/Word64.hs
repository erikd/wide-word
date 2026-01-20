{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.WideWord.Word64
--
-- Maintainer  :  erikd@mega-nerd.com
-- Stability   :  experimental
-- Portability :  non-portable (GHC extensions and primops)
--
-- This module provides an opaque unsigned 64 bit value with the usual set
-- of typeclass instances one would expect for a fixed width unsigned integer
-- type.
-- Operations like addition, subtraction and multiplication etc provide a
-- "modulo 2^64" result as one would expect from a fixed width unsigned word.
--
-- This just re-exports the Word64 type defined in Data.Word plus some functions
-- like plusCarrySum and timesCarryProd that do the normal addition and multiplication
-- but provide a carry in addition to the regular operation.
-------------------------------------------------------------------------------

#include <MachDeps.h>

module Data.WideWord.Word64
  ( mkWord64
  , oneWord64
  , plusCarrySum
  , quotRem2Word64
  , showHexWord64
  , subCarryDiff
  , timesCarryProd
  , word64Hi32
  , word64Lo32
  , zeroWord64
  ) where

import Data.Bits (shiftL, shiftR)

#if WORD_SIZE_IN_BITS == 32
import GHC.Exts (Word#, Word64#, uncheckedShiftRL64#, word64ToWord#)
#else
import Data.WideWord.Compat
#endif

import GHC.Word (Word32 (..), Word64 (..))

import Numeric (showHex)

{-# INLINE mkWord64 #-}
mkWord64 :: Word32 -> Word32 -> Word64
mkWord64 hi lo = fromIntegral hi `shiftL` 32 + fromIntegral lo

{-# INLINE showHexWord64 #-}
showHexWord64 :: Word64 -> String
showHexWord64 w = showHex w ""

{-# INLINE word64Hi32 #-}
word64Hi32 :: Word64 -> Word32
word64Hi32 w = fromIntegral (w `shiftR` 32)

{-# INLINE word64Lo32 #-}
word64Lo32 :: Word64 -> Word32
word64Lo32 = fromIntegral

{-# INLINE oneWord64 #-}
oneWord64 :: Word64
oneWord64 = 1

{-# INLINE zeroWord64 #-}
zeroWord64 :: Word64
zeroWord64 = 0

#if WORD_SIZE_IN_BITS == 64

{-# INLINE plusCarrySum #-}
plusCarrySum :: Word64 -> Word64 -> (Word64, Word64)
plusCarrySum (W64# a) (W64# b) =
  let !(# c, s #) = plusWord2# a b
   in (W64# c, W64# s)

quotRem2Word64 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
quotRem2Word64 (W64# n1) (W64# n0) (W64# d) =
  case quotRemWord2# n1 n0 d of
    (# q, r #) -> (W64# q, W64# r)

{-# INLINE subCarryDiff #-}
subCarryDiff :: Word64 -> Word64 -> (Word64, Word64)
subCarryDiff (W64# a) (W64# b) =
  let !(# d, c #) = subWordC# a b
   in (W64# (int2Word# c), W64# d)

{-# INLINE timesCarryProd #-}
timesCarryProd :: Word64 -> Word64 -> (Word64, Word64)
timesCarryProd (W64# a) (W64# b) =
  let !(# c, s #) = timesWord2# a b
   in (W64# c, W64# s)

#elif  WORD_SIZE_IN_BITS == 32

{-# INLINE plusCarrySum #-}
plusCarrySum :: Word64 -> Word64 -> (Word64, Word64)
plusCarrySum a b = (if ab < a then 1 else 0, ab)
  where
    ab = a + b

quotRem2Word64 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
quotRem2Word64 n1 n0 d =
  -- This is correct, but sub-optimal and I could not be bothered writing an
  -- optimal version that is only needed for 32 bit systems.
  case quotRem (toInteger n1 `shiftL` 64 + toInteger n0) (toInteger d) of
    (q, r) -> (fromInteger q, fromInteger r)

{-# INLINE subCarryDiff #-}
subCarryDiff :: Word64 -> Word64 -> (Word64, Word64)
subCarryDiff a b = (if ab > a then 1 else 0, ab)
  where
    ab = a - b

pattern W64 :: Word32 -> Word32 -> Word64
pattern W64 hi lo <- ((\x -> (word64Hi32 x, word64Lo32 x)) -> (hi, lo))
  where
    W64 hi lo = mkWord64 hi lo
{-# COMPLETE W64 #-}

{-# INLINE timesCarryProd #-}
timesCarryProd :: Word64 -> Word64 -> (Word64, Word64)
timesCarryProd (W64 a1 a0) (W64 b1 b0) = (W64 p3 p2, W64 p1 p0)
  where
    W64 c00 p00 = fromIntegral a0 * fromIntegral b0
    W64 c01 p01 = fromIntegral a0 * fromIntegral b1
    W64 c10 p10 = fromIntegral a1 * fromIntegral b0
    W64 c11 p11 = fromIntegral a1 * fromIntegral b1

    p0 = p00
    W64 c1 p1 = fromIntegral c00 + fromIntegral p01 + fromIntegral p10
    W64 c2 p2 = fromIntegral c01 + fromIntegral c10 + fromIntegral p11 + fromIntegral c1
    p3 = c11 + c2

#else

error "Sorry, this package only supports 32 and 64 bit word sizes."

#endif
