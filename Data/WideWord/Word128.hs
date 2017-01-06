{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
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
  , toInteger128
  , zeroWord128
  ) where

import Data.Bits (shiftL, shiftR)

import GHC.Base
import GHC.Word (Word64 (..), byteSwap64)

import Numeric (showHex)


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

instance Bounded Word128 where
  minBound = zeroWord128
  maxBound = Word128 maxBound maxBound

instance Enum Word128 where
  succ = succ128
  pred = pred128
  toEnum = toEnum128
  fromEnum = fromEnum128

instance Num Word128 where
  (+) = plus128
  (-) = minus128
  (*) = times128
  negate = negate128
  abs = id
  signum = signum128
  fromInteger = fromInteger128

-- -----------------------------------------------------------------------------
-- Functions for `Ord` instance.

compare128 :: Word128 -> Word128 -> Ordering
compare128 (Word128 a1 a0) (Word128 b1 b0) =
  case compare a1 b1 of
    EQ -> compare a0 b0
    LT -> LT
    GT -> GT

-- -----------------------------------------------------------------------------
-- Functions for `Enum` instance.

{-# INLINABLE succ128 #-}
succ128 :: Word128 -> Word128
succ128 (Word128 a1 a0) =
  case a0 + 1 of
    0 -> Word128 (a1 + 1) 0
    s -> Word128 a1 s

{-# INLINABLE pred128 #-}
pred128 :: Word128 -> Word128
pred128 (Word128 a1 a0) =
  case a0 of
    0 -> Word128 (a1 - 1) maxBound
    _ -> Word128 a1 (a0 - 1)

{-# INLINABLE toEnum128 #-}
toEnum128 :: Int -> Word128
toEnum128 i = Word128 0 (toEnum i)

{-# INLINABLE fromEnum128 #-}
fromEnum128 :: Word128 -> Int
fromEnum128 (Word128 _ a0) = fromEnum a0

-- -----------------------------------------------------------------------------
-- Functions for `Num` instance.

{-# INLINABLE plus128 #-}
plus128 :: Word128 -> Word128 -> Word128
plus128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# s1) (W64# s0)
  where
    (# c1, s0 #) = plusWord2# a0 b0
    s1a = plusWord# a1 b1
    s1 = plusWord# c1 s1a

{-# INLINABLE minus128 #-}
minus128 :: Word128 -> Word128 -> Word128
minus128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# d1) (W64# d0)
  where
    (# d0, c1 #) = subWordC# a0 b0
    a1c = minusWord# a1 (int2Word# c1)
    d1 = minusWord# a1c b1

times128 :: Word128 -> Word128 -> Word128
times128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# p1) (W64# p0)
  where
    (# c1, p0 #) = timesWord2# a0 b0
    p1a = timesWord# a1 b0
    p1b = timesWord# a0 b1
    p1c = plusWord# p1a p1b
    p1 = plusWord# p1c c1

{-# INLINABLE negate128 #-}
negate128 :: Word128 -> Word128
negate128 (Word128 (W64# a1) (W64# a0)) =
  case plusWord2# (not# a0) 1## of
    (# c, s #) -> Word128 (W64# (plusWord# (not# a1) c)) (W64# s)

{-# INLINABLE signum128 #-}
signum128 :: Word128 -> Word128
signum128 (Word128 (W64# 0##) (W64# 0##)) = zeroWord128
signum128 _ = oneWord128

fromInteger128 :: Integer -> Word128
fromInteger128 i =
  Word128 (fromIntegral $ i `shiftR` 64) (fromIntegral i)

-- -----------------------------------------------------------------------------
-- Helpers.

toInteger128 :: Word128 -> Integer
toInteger128 (Word128 a1 a0) = fromIntegral a1 `shiftL` 64 + fromIntegral a0

-- -----------------------------------------------------------------------------
-- Constants.

zeroWord128 :: Word128
zeroWord128 = Word128 0 0

oneWord128 :: Word128
oneWord128 = Word128 0 1

