{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
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

import Data.WideWord.Compat

#if WORD_SIZE_IN_BITS == 32
import GHC.Exts (Word#, Word32#, Word64#, uncheckedShiftRL64#, word64ToWord#, wordToWord32#)
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
plusCarrySum (W64# a) (W64# b) =
    (mkWord64 0 (W32# (word64ToWord32# c2)), mkWord64 (W32# (word64ToWord32# s1)) (W32# (word64ToWord32# s0)))
  where
    !(# a1, a0 #) = (# word64ToHiWord# a, word64ToWord# a #)
    !(# b1, b0 #) = (# word64ToHiWord# b, word64ToWord# b #)
    !(# c1, s0 #) = plusWord2# a b
    !(# c2a, s1a #) = plusWord2# b c1
    !(# c2b, s1 #) = plusWord2# a s1a
    !c2 = plusWord# c2a c2b

quotRem2Word64 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
quotRem2Word64 n1 n0 d =
  -- This is correct, but sub-optimal and I could not be bothered writing an
  -- optimal version that is only needed for 32 bit systems.
  case quotRem (toInteger n1 `shiftL` 64 + toInteger n0) (toInteger d) of
    (q, r) -> (fromInteger q, fromInteger r)

{-# INLINE subCarryDiff #-}
subCarryDiff :: Word64 -> Word64 -> (Word64, Word64)
subCarryDiff (W64# a) (W64# b) =
    (mkWord64 0 (W32# (wordToWord32# c2)), mkWord64 (W32# (wordToWord32# d1)) (W32# (wordToWord32# d0)))
  where
    !(# a1, a0 #) = (# word64ToHiWord# a, word64ToWord# a #)
    !(# b1, b0 #) = (# word64ToHiWord# b, word64ToWord# b #)
    !(# d0, c1 #) = subWordC# a0 b0
    !(# c2a, b1a #) = plusWord2# b1 (int2Word# c1)
    !(# d1, c2b #) = subWordC# a1 b1a
    !c2 = plusWord# c2a (int2Word# c2b)

-- plusWord2# :: Word# -> Word# -> (# Word#, Word# #)
-- subWordC# :: Word# -> Word# -> (# Word#, Int# #)

{-# INLINE timesCarryProd #-}
timesCarryProd :: Word64 -> Word64 -> (Word64, Word64)
timesCarryProd (W64# a) (W64# b) =
    (mkWord64 (W32# (word64ToWord32# p3)) (W32# (word64ToWord32# p2)), mkWord64 (W32# (word64ToWord32# p1)) (W32# (word64ToWord32# p0)))
  where
    !(# a1, a0 #) = (# word64ToHiWord# a, word64ToWord# a #)
    !(# b1, b0 #) = (# word64ToHiWord# b, word64ToWord# b #)

    !(# c1a, p0 #) = timesWord2# a b

    !(# c2a, p1a #) = timesWord2# a b
    !(# c2b, p1b #) = timesWord2# a b
    !(# c2c, p1c #) = plusWord2# p1a p1b
    !(# c2d, p1 #) = plusWord2# p1c c1a

    !(# c3a, p2a #) = timesWord2# a b
    !(# c3b, p2b #) = plusWord2# p2a c2a
    !(# c3c, p2c #) = plusWord2# p2b c2b
    !(# c3d, p2d #) = plusWord2# p2c c2c
    !(# c3e, p2 #) = plusWord2# p2d c2d

    !p3 = c3a `plusWord#` c3b `plusWord#` c3c `plusWord#` c3d `plusWord#` c3e

{-# INLINE word64ToHiWord# #-}
word64ToHiWord# :: Word64# -> Word#
word64ToHiWord# w = word64ToWord# (w `uncheckedShiftRL64#` 32#)

{-# INLINE word64ToWord32# #-}
word64ToWord32# :: Word64# -> Word32#
word64ToWord32# w =  wordToWord32# (word64ToWord# w)

#else

error "Sorry, this package only supports 32 and 64 bit word sizes."

#endif
