{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | This module provides an opaque unsigned 128 bit value with the usual set
-- of typeclass instances one would expect for a fixed width unsigned integer
-- type.
-- Operations like addition, subtraction and multiplication etc provide a
-- "modulo 2^128" result as one would expect from a fixed width unsigned word.

module Data.WideWord.Word128
  ( Word128 (..)
  , byteSwapWord128
  , showHexWord128
  , zeroWord128
  ) where

import Data.Bits (shiftL, shiftR)

import Numeric (showHex)

import GHC.Word (Word64 (..), byteSwap64)


data Word128 = Word128
  { word128Hi64 :: {-# UNPACK #-} !Word64
  , word128Lo64 :: {-# UNPACK #-} !Word64
  }
  deriving Eq


byteSwapWord128 :: Word128 -> Word128
byteSwapWord128 (Word128 a1 a0) = Word128 (byteSwap64 a1) (byteSwap64 a0)


showHexWord128 :: Word128 -> String
showHexWord128 (Word128 a1 a0)
  | a1 == 0 = showHex a0 ""
  | otherwise = showHex a1 zeros ++ showHex a0 ""
  where
    h0 = showHex a0 ""
    zeros = replicate (16 - length h0) '0'

instance Show Word128 where
  show = show . toInteger128

instance Read Word128 where
  readsPrec p s = [(fromInteger128 (x :: Integer), r) | (x, r) <- readsPrec p s]

instance Ord Word128 where
  compare = compare128

-- -----------------------------------------------------------------------------
-- Functions for `Ord` instance.

compare128 :: Word128 -> Word128 -> Ordering
compare128 (Word128 a1 a0) (Word128 b1 b0) =
  case compare a1 b1 of
    EQ -> compare a0 b0
    LT -> LT
    GT -> GT

-- -----------------------------------------------------------------------------
-- Helpers.

toInteger128 :: Word128 -> Integer
toInteger128 (Word128 a1 a0) = fromIntegral a1 `shiftL` 64 + fromIntegral a0

fromInteger128 :: Integer -> Word128
fromInteger128 i = Word128 (fromIntegral (i `shiftR` 64)) (fromIntegral i)

-- -----------------------------------------------------------------------------
-- Constants.

zeroWord128 :: Word128
zeroWord128 = Word128 0 0
