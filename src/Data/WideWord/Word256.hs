{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  Data.WideWord.Word256
----
---- Maintainer  :  erikd@mega-nerd.com
---- Stability   :  experimental
---- Portability :  non-portable (GHC extensions and primops)
----
---- This module provides an opaque unsigned 256 bit value with the usual set
---- of typeclass instances one would expect for a fixed width unsigned integer
---- type.
---- Operations like addition, subtraction and multiplication etc provide a
---- "modulo 2^256" result as one would expect from a fixed width unsigned word.
-------------------------------------------------------------------------------

#include <MachDeps.h>

module Data.WideWord.Word256
  ( Word256 (..)
  , showHexWord256
  , zeroWord256
  ) where

import Control.DeepSeq (NFData (..))

import Data.Bits (Bits (..), FiniteBits (..), shiftL)
import Data.Data (Data, Typeable)
import Data.Ix (Ix)
#if ! MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

import GHC.Base (Int (..), and#, int2Word#, minusWord#, not#, or#, plusWord#, plusWord2#
                , subWordC#, timesWord#, timesWord2#, xor#)
import GHC.Enum (predError, succError)
import GHC.Exts ((*#), (+#), Int#, State#, ByteArray#, MutableByteArray#, Addr#)
import GHC.Real ((%))
import GHC.Word (Word64 (..), Word32)
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Numeric (showHex)

import Data.Primitive.Types (Prim (..), defaultSetByteArray#, defaultSetOffAddr#)

data Word256 = Word256
  { word256hi :: {-# UNPACK #-} !Word64
  , word256m1 :: {-# UNPACK #-} !Word64
  , word256m0 :: {-# UNPACK #-} !Word64
  , word256lo :: {-# UNPACK #-} !Word64
  }
  deriving (Eq, Data, Ix, Typeable)

showHexWord256 :: Word256 -> String
showHexWord256 (Word256 a3 a2 a1 a0)
  | a3 == 0 = if a2 == 0
      then if a1 == 0
        then showHex a0 ""
        else showHex a1 zeros0 ++ showHex a0 ""
      else showHex a2 zeros1 ++ showHex a1 zeros0 ++ showHex a0 ""
  | otherwise =
         showHex a3 zeros2 ++ showHex a2 zeros1
      ++ showHex a1 zeros0 ++ showHex a0 ""
  where
    h0 = showHex a0 ""
    h1 = showHex a1 ""
    h2 = showHex a2 ""
    zeros0 = replicate (16 - length h0) '0'
    zeros1 = replicate (16 - length h1) '0'
    zeros2 = replicate (16 - length h2) '0'

instance Show Word256 where
  show = show . toInteger256

instance Read Word256 where
  readsPrec p s = [(fromInteger256 (x :: Integer), r) | (x, r) <- readsPrec p s]

instance Ord Word256 where
  compare = compare256

instance Bounded Word256 where
  minBound = zeroWord256
  maxBound = Word256 maxBound maxBound maxBound maxBound

instance Enum Word256 where
  succ = succ256
  pred = pred256
  toEnum = toEnum256
  fromEnum = fromEnum256

instance Num Word256 where
  (+) = plus256
  (-) = minus256
  (*) = times256
  negate = negate256
  abs = id
  signum = signum256
  fromInteger = fromInteger256

instance Bits Word256 where
  (.&.) = and256
  (.|.) = or256
  xor = xor256
  complement = complement256
  shiftL = shiftL256
  unsafeShiftL = shiftL256
  shiftR = shiftR256
  unsafeShiftR = shiftR256
  rotateL = rotateL256
  rotateR = rotateR256

  bitSize _ = 256
  bitSizeMaybe _ = Just 256
  isSigned _ = False

  testBit = testBit256
  bit = bit256

  popCount = popCount256

instance FiniteBits Word256 where
  finiteBitSize _ = 256
  countLeadingZeros = countLeadingZeros256
  countTrailingZeros = countTrailingZeros256

instance Real Word256 where
  toRational x = toInteger256 x % 1

instance Integral Word256 where
  quot n d = fst (quotRem256 n d)
  rem n d = snd (quotRem256 n d)
  div n d = fst (quotRem256 n d)
  mod n d = snd (quotRem256 n d)
  quotRem = quotRem256
  divMod = quotRem256
  toInteger = toInteger256

instance Storable Word256 where
  sizeOf _ = 4 * sizeOf (0 :: Word64)
  alignment _ = 4 * alignment (0 :: Word64)
  peek = peek256
  peekElemOff = peekElemOff256
  poke = poke256
  pokeElemOff = pokeElemOff256

instance NFData Word256 where
  -- The fields are already strict and unpacked, so do nothing.
  rnf !_ = ()

instance Prim Word256 where
  sizeOf#         = sizeOf256#
  alignment#      = alignment256#
  indexByteArray# = indexByteArray256#
  readByteArray#  = readByteArray256#
  writeByteArray# = writeByteArray256#
  setByteArray#   = setByteArray256#
  indexOffAddr#   = indexOffAddr256#
  readOffAddr#    = readOffAddr256#
  writeOffAddr#   = writeOffAddr256#
  setOffAddr#     = setOffAddr256#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

-- -----------------------------------------------------------------------------
-- Rewrite rules.

{-# RULES
"fromIntegral :: Word256 -> Word256" fromIntegral = id :: Word256 -> Word256

#if WORD_SIZE_IN_BITS < 64
"fromIntegral :: Int -> Word256"     fromIntegral = \(I# i#) -> Word256 (W64# (wordToWord64# 0##)) (W64# (wordToWord64# 0##)) (W64# (wordToWord64# 0##)) (W64# (wordToWord64# (int2Word# i#)))
#else 
"fromIntegral :: Int -> Word256"     fromIntegral = \(I# i#) -> Word256 (W64# 0##) (W64# 0##) (W64# 0##) (W64# (int2Word# i#))
#endif 
"fromIntegral :: Word -> Word256"    fromIntegral = Word256 0 0 0 . (fromIntegral :: Word -> Word64)
"fromIntegral :: Word32 -> Word256"  fromIntegral = Word256 0 0 0 . (fromIntegral :: Word32 -> Word64)
"fromIntegral :: Word64 -> Word256"  fromIntegral = Word256 0 0 0

"fromIntegral :: Word256 -> Int"     fromIntegral = \(Word256 _ _ _ w) -> fromIntegral w :: Int
"fromIntegral :: Word256 -> Word"    fromIntegral = \(Word256 _ _ _ w) -> fromIntegral w :: Word
"fromIntegral :: Word256 -> Word32"  fromIntegral = \(Word256 _ _ _ w) -> fromIntegral w :: Word32
"fromIntegral :: Word256 -> Word64"  fromIntegral = \(Word256 _ _ _ w) -> w
  #-}

-- -----------------------------------------------------------------------------
-- Functions for `Ord` instance.

compare256 :: Word256 -> Word256 -> Ordering
compare256 (Word256 a3 a2 a1 a0) (Word256 b3 b2 b1 b0) =
  compare a3 b3 <> compare a2 b2 <> compare a1 b1 <> compare a0 b0

-- -----------------------------------------------------------------------------
-- Functions for `Enum` instance.

succ256 :: Word256 -> Word256
succ256 (Word256 a3 a2 a1 a0)
  | a0 == maxBound = if a1 == maxBound
      then if a2 == maxBound
        then if a3 == maxBound
          then succError "Word256"
          else Word256 (a3 + 1) 0 0 0
        else Word256 a3 (a2 + 1) 0 0
      else Word256 a3 a2 (a1 + 1) 0
  | otherwise = Word256 a3 a2 a1 (a0 + 1)


pred256 :: Word256 -> Word256
pred256 (Word256 a3 a2 a1 a0)
  | a0 == 0 = if a1 == 0
      then if a2 == 0
        then if a3 == 0
          then predError "Word256"
          else Word256 (a3 - 1) maxBound maxBound maxBound
        else Word256 a3 (a2 - 1) maxBound maxBound
      else Word256 a3 a2 (a1 - 1) maxBound
  | otherwise = Word256 a3 a2 a1 (a0 - 1)


{-# INLINABLE toEnum256 #-}
toEnum256 :: Int -> Word256
toEnum256 i = Word256 0 0 0 (toEnum i)

{-# INLINABLE fromEnum256 #-}
fromEnum256 :: Word256 -> Int
fromEnum256 (Word256 _ _ _ a0) = fromEnum a0

-- -----------------------------------------------------------------------------
-- Functions for `Num` instance.

{-# INLINABLE plus256 #-}
plus256 :: Word256 -> Word256 -> Word256
plus256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
        (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
#if WORD_SIZE_IN_BITS < 64
  Word256 (W64# (wordToWord64# s3)) (W64# (wordToWord64# s2)) (W64# (wordToWord64# s1)) (W64# (wordToWord64# s0))
  where
    !(# c1, s0 #) = plusWord2# (word64ToWord# a0) (word64ToWord# b0)
    !(# c2a, s1a #) = plusWord2# (word64ToWord# a1) (word64ToWord# b1)
    !(# c2b, s1 #) = plusWord2# s1a c1
    c2 = plusWord# c2a c2b
    !(# c3a, s2a #) = plusWord2# (word64ToWord# a2) (word64ToWord# b2)
    !(# c3b, s2 #) = plusWord2# s2a c2
    c3 = plusWord# c3a c3b
    s3 = plusWord# (word64ToWord# a3) (plusWord# (word64ToWord# b3) c3)
#else 
  Word256 (W64# s3) (W64# s2) (W64# s1) (W64# s0)
  where
    !(# c1, s0 #) = plusWord2# a0 b0
    !(# c2a, s1a #) = plusWord2# a1 b1
    !(# c2b, s1 #) = plusWord2# s1a c1
    c2 = plusWord# c2a c2b
    !(# c3a, s2a #) = plusWord2# a2 b2
    !(# c3b, s2 #) = plusWord2# s2a c2
    c3 = plusWord# c3a c3b
    s3 = plusWord# a3 (plusWord# b3 c3)
#endif 

{-# INLINABLE minus256 #-}
minus256 :: Word256 -> Word256 -> Word256
minus256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
         (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
#if WORD_SIZE_IN_BITS < 64
  Word256 (W64# (wordToWord64# s3)) (W64# (wordToWord64# s2)) (W64# (wordToWord64# s1)) (W64# (wordToWord64# s0))
  where
    !(# s0, v1 #) = subWordC# (word64ToWord# a0) (word64ToWord# b0)
    !(# s1, v2 #) =
      case v1 of
        0# -> subWordC# (word64ToWord# a1) (word64ToWord# b1)
        _ ->
          case word64ToWord# a1 of
            0## -> (# minusWord# 0xFFFFFFFFFFFFFFFF## (word64ToWord#b1), 1# #)
            _ -> subWordC# (minusWord# (word64ToWord# a1) 1##) (word64ToWord# b1)
    !(# s2, v3 #) =
      case v2 of
        0# -> subWordC# (word64ToWord# a2) (word64ToWord# b2)
        _ ->
          case word64ToWord# a2 of
            0## -> (# minusWord# 0xFFFFFFFFFFFFFFFF## (word64ToWord# b2), 1# #)
            _ -> subWordC# (minusWord# (word64ToWord# a2) 1##) (word64ToWord# b2)
    !s3 =
      case v3 of
        0# -> minusWord# (word64ToWord# a3) (word64ToWord# b3)
        _ -> minusWord# (minusWord# (word64ToWord# a3) 1##) (word64ToWord# b3)
#else 
  Word256 (W64# s3) (W64# s2) (W64# s1) (W64# s0)
  where
    !(# s0, v1 #) = subWordC# a0 b0
    !(# s1, v2 #) =
      case v1 of
        0# -> subWordC# a1 b1
        _ ->
          case a1 of
            0## -> (# minusWord# 0xFFFFFFFFFFFFFFFF## b1, 1# #)
            _ -> subWordC# (minusWord# a1 1##) b1
    !(# s2, v3 #) =
      case v2 of
        0# -> subWordC# a2 b2
        _ ->
          case a2 of
            0## -> (# minusWord# 0xFFFFFFFFFFFFFFFF## b2, 1# #)
            _ -> subWordC# (minusWord# a2 1##) b2
    !s3 =
      case v3 of
        0# -> minusWord# a3 b3
        _ -> minusWord# (minusWord# a3 1##) b3
#endif 

times256 :: Word256 -> Word256 -> Word256
times256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
         (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
#if WORD_SIZE_IN_BITS < 64
  Word256 (W64# (wordToWord64# r3)) (W64# (wordToWord64# r2)) (W64# (wordToWord64# r1)) (W64# (wordToWord64# r0))
  where
    !(# c00, p00 #) = timesWord2# (word64ToWord# a0) (word64ToWord# b0)
    !(# c01, p01 #) = timesWord2# (word64ToWord# a0) (word64ToWord# b1)
    !(# c02, p02 #) = timesWord2# (word64ToWord# a0) (word64ToWord# b2)
    !p03 = timesWord# (word64ToWord# a0) (word64ToWord# b3)
    !(# c10, p10 #) = timesWord2# (word64ToWord# a1) (word64ToWord# b0)
    !(# c11, p11 #) = timesWord2# (word64ToWord# a1) (word64ToWord# b1)
    !p12 = timesWord# (word64ToWord# a1) (word64ToWord# b2)
    !(# c20, p20 #) = timesWord2# (word64ToWord# a2) (word64ToWord# b0)
    !p21 = timesWord# (word64ToWord# a2) (word64ToWord# b1)
    !p30 = timesWord# (word64ToWord# a3) (word64ToWord# b0)
    !r0 = p00
    !c1 = c00
    !(# c2x, r1a #) = plusWord2# p01 p10
    !(# c2y, r1b #) = plusWord2# r1a c1
    !(# c3w, c2 #) = plusWord2# c2x c2y
    !r1 = r1b
    !(# c3x, r2a #) = plusWord2# p11 p20
    !(# c3y, r2b #) = plusWord2# p02 r2a
    !(# c3z, r2c #) = plusWord2# r2b c2
    !(# c3s, r2d #) = plusWord2# r2c c01
    !(# c3t, r2e #) = plusWord2# r2d c10
    !r2 = r2e
    !r3 = p30 `plusWord#` p21 `plusWord#` p12 `plusWord#`
         p03 `plusWord#` c3w `plusWord#` c3x `plusWord#`
         c3y `plusWord#` c3z `plusWord#` c3s `plusWord#`
         c3t `plusWord#` c02 `plusWord#` c11 `plusWord#`
         c20
#else 
  Word256 (W64# r3) (W64# r2) (W64# r1) (W64# r0)
  where
    !(# c00, p00 #) = timesWord2# a0 b0
    !(# c01, p01 #) = timesWord2# a0 b1
    !(# c02, p02 #) = timesWord2# a0 b2
    !p03 = timesWord# a0 b3
    !(# c10, p10 #) = timesWord2# a1 b0
    !(# c11, p11 #) = timesWord2# a1 b1
    !p12 = timesWord# a1 b2
    !(# c20, p20 #) = timesWord2# a2 b0
    !p21 = timesWord# a2 b1
    !p30 = timesWord# a3 b0
    !r0 = p00
    !c1 = c00
    !(# c2x, r1a #) = plusWord2# p01 p10
    !(# c2y, r1b #) = plusWord2# r1a c1
    !(# c3w, c2 #) = plusWord2# c2x c2y
    !r1 = r1b
    !(# c3x, r2a #) = plusWord2# p11 p20
    !(# c3y, r2b #) = plusWord2# p02 r2a
    !(# c3z, r2c #) = plusWord2# r2b c2
    !(# c3s, r2d #) = plusWord2# r2c c01
    !(# c3t, r2e #) = plusWord2# r2d c10
    !r2 = r2e
    !r3 = p30 `plusWord#` p21 `plusWord#` p12 `plusWord#`
         p03 `plusWord#` c3w `plusWord#` c3x `plusWord#`
         c3y `plusWord#` c3z `plusWord#` c3s `plusWord#`
         c3t `plusWord#` c02 `plusWord#` c11 `plusWord#`
         c20
#endif 

{-# INLINABLE negate256 #-}
negate256 :: Word256 -> Word256
negate256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0)) =
#if WORD_SIZE_IN_BITS < 64
  case plusWord2# (not# (word64ToWord# a0)) 1## of
    (# c1, s0 #) -> case plusWord2# (not# (word64ToWord# a1)) c1 of
      (# c2, s1 #) -> case plusWord2# (not# (word64ToWord# a2)) c2 of
        (# c3, s2 #) -> case plusWord# (not# (word64ToWord# a3)) c3 of
          s3 -> Word256 (W64# (wordToWord64# s3)) (W64# (wordToWord64# s2)) (W64# (wordToWord64# s1)) (W64# (wordToWord64# s0))
#else 
  case plusWord2# (not# a0) 1## of
    (# c1, s0 #) -> case plusWord2# (not# a1) c1 of
      (# c2, s1 #) -> case plusWord2# (not# a2) c2 of
        (# c3, s2 #) -> case plusWord# (not# a3) c3 of
          s3 -> Word256 (W64# s3) (W64# s2) (W64# s1) (W64# s0)
#endif 

{-# INLINABLE signum256 #-}
signum256 :: Word256 -> Word256
#if WORD_SIZE_IN_BITS < 64
signum256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0)) 
  | 0## <- word64ToWord# a3
  , 0## <- word64ToWord# a2 
  , 0## <- word64ToWord# a1
  , 0## <- word64ToWord# a0 = zeroWord256
  | otherwise = oneWord256
#else 
signum256 (Word256 (W64# 0##) (W64# 0##) (W64# 0##) (W64# 0##)) = zeroWord256
signum256 _ = oneWord256
#endif 

fromInteger256 :: Integer -> Word256
fromInteger256 i = Word256
  (fromInteger $ i `shiftR` 192)
  (fromInteger $ i `shiftR` 128)
  (fromInteger $ i `shiftR` 64)
  (fromInteger i)

-- -----------------------------------------------------------------------------
-- Functions for `Bits` instance.

{-# INLINABLE and256 #-}
and256 :: Word256 -> Word256 -> Word256
and256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
       (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
#if WORD_SIZE_IN_BITS < 64
  Word256 (W64# (wordToWord64# (and# (word64ToWord# a3) (word64ToWord# b3)))) (W64# (wordToWord64# (and# (word64ToWord# a2) (word64ToWord# b2))))
          (W64# (wordToWord64# (and# (word64ToWord# a1) (word64ToWord# b1)))) (W64# (wordToWord64# (and# (word64ToWord# a0) (word64ToWord# b0))))
#else
  Word256 (W64# (and# a3 b3)) (W64# (and# a2 b2))
          (W64# (and# a1 b1)) (W64# (and# a0 b0))
#endif

{-# INLINABLE or256 #-}
or256 :: Word256 -> Word256 -> Word256
or256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
      (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
#if WORD_SIZE_IN_BITS < 64
  Word256 (W64# (wordToWord64# (or# (word64ToWord# a3) (word64ToWord# b3)))) (W64# (wordToWord64# (or# (word64ToWord# a2) (word64ToWord# b2))))
          (W64# (wordToWord64# (or# (word64ToWord# a1) (word64ToWord# b1)))) (W64# (wordToWord64# (or# (word64ToWord# a0) (word64ToWord# b0))))
#else
  Word256 (W64# (or# a3 b3)) (W64# (or# a2 b2))
          (W64# (or# a1 b1)) (W64# (or# a0 b0))
#endif


{-# INLINABLE xor256 #-}
xor256 :: Word256 -> Word256 -> Word256
xor256 (Word256 (W64# a3) (W64# a2) (W64# a1) (W64# a0))
       (Word256 (W64# b3) (W64# b2) (W64# b1) (W64# b0)) =
#if WORD_SIZE_IN_BITS < 64
  Word256 (W64# (wordToWord64# (xor# (word64ToWord# a3) (word64ToWord# b3)))) (W64# (wordToWord64# (xor# (word64ToWord# a2) (word64ToWord# b2))))
          (W64# (wordToWord64# (xor# (word64ToWord# a1) (word64ToWord# b1)))) (W64# (wordToWord64# (xor# (word64ToWord# a0) (word64ToWord# b0))))
#else
  Word256 (W64# (xor# a3 b3)) (W64# (xor# a2 b2))
          (W64# (xor# a1 b1)) (W64# (xor# a0 b0))
#endif

{-# INLINABLE complement256 #-}
complement256 :: Word256 -> Word256
complement256 (Word256 a3 a2 a1 a0) = Word256
  (complement a3) (complement a2)
  (complement a1) (complement a0)

-- Probably not worth inlining this.
shiftL256 :: Word256 -> Int -> Word256
shiftL256 w@(Word256 a3 a2 a1 a0) s
  | s < 0 || s >= 256 = zeroWord256
  | s == 0 = w
  | s > 192 = Word256 (a0 `shiftL` (s - 192)) 0 0 0
  | s == 192 = Word256 a0 0 0 0
  | s > 128 = Word256
      (a1 `shiftL` (s - 128) + a0 `shiftR` (192 - s))
      (a0 `shiftL` (s - 128))
      0 0
  | s == 128 = Word256 a1 a0 0 0
  | s > 64 = Word256
      (a2 `shiftL` (s - 64) + a1 `shiftR` (128 - s))
      (a1 `shiftL` (s - 64) + a0 `shiftR` (128 - s))
      (a0 `shiftL` (s - 64))
      0
  | s == 64 = Word256 a2 a1 a0 0
  | otherwise = Word256
      (a3 `shiftL` s + a2 `shiftR` (64 - s))
      (a2 `shiftL` s + a1 `shiftR` (64 - s))
      (a1 `shiftL` s + a0 `shiftR` (64 - s))
      (a0 `shiftL` s)

shiftR256 :: Word256 -> Int -> Word256
shiftR256 w@(Word256 a3 a2 a1 a0) s
  | s < 0 = zeroWord256
  | s == 0 = w
  | s >= 256 = zeroWord256
  | s > 192 = Word256 0 0 0 (a3 `shiftR` (s - 192))
  | s == 192 = Word256 0 0 0 a3
  | s > 128 = Word256 0 0
      (a3 `shiftR` (s - 128))
      (a2 `shiftR` (s - 128) + a3 `shiftL` (192 - s))
  | s == 128 = Word256 0 0 a3 a2
  | s > 64 = Word256 0
      (a3 `shiftR` (s - 64))
      (a2 `shiftR` (s - 64) + a3 `shiftL` (128 - s))
      (a1 `shiftR` (s - 64) + a2 `shiftL` (128 - s))
  | s == 64 = Word256 0 a3 a2 a1
  | otherwise = Word256
      (a3 `shiftR` s)
      (a2 `shiftR` s + a3 `shiftL` (64 - s))
      (a1 `shiftR` s + a2 `shiftL` (64 - s))
      (a0 `shiftR` s + a1 `shiftL` (64 - s))

rotateL256 :: Word256 -> Int -> Word256
rotateL256 w@(Word256 a3 a2 a1 a0) r
  | r < 0 = zeroWord256
  | r == 0 = w
  | r >= 256 = rotateL256 w (r `mod` 256)
  | r >= 64 = rotateL256 (Word256 a2 a1 a0 a3) (r - 64)
  | otherwise =
      Word256 s3 s2 s1 s0
      where
        s0 = a0 `shiftL` r + a3 `shiftR` (64 - r)
        s1 = a1 `shiftL` r + a0 `shiftR` (64 - r)
        s2 = a2 `shiftL` r + a1 `shiftR` (64 - r)
        s3 = a3 `shiftL` r + a2 `shiftR` (64 - r)

rotateR256 :: Word256 -> Int -> Word256
rotateR256 w@(Word256 a3 a2 a1 a0) r
  | r < 0 = rotateR256 w (256 - (abs r `mod` 256))
  | r == 0 = w
  | r >= 256 = rotateR256 w (r `mod` 256)
  | r >= 64 = rotateR256 (Word256 a0 a3 a2 a1) (r - 64)
  | otherwise =
      Word256 s3 s2 s1 s0
      where
        s0 = a0 `shiftR` r + a1 `shiftL` (64 - r)
        s1 = a1 `shiftR` r + a2 `shiftL` (64 - r)
        s2 = a2 `shiftR` r + a3 `shiftL` (64 - r)
        s3 = a3 `shiftR` r + a0 `shiftL` (64 - r)

testBit256 :: Word256 -> Int -> Bool
testBit256 (Word256 a3 a2 a1 a0) i
  | i < 0 = False
  | i >= 256 = False
  | i >= 192 = testBit a3 (i - 192)
  | i >= 128 = testBit a2 (i - 128)
  | i >= 64 = testBit a1 (i - 64)
  | otherwise = testBit a0 i

bit256 :: Int -> Word256
bit256 indx
  | indx < 0 = zeroWord256
  | indx >= 256 = zeroWord256
  | otherwise = shiftL256 oneWord256 indx

popCount256 :: Word256 -> Int
popCount256 (Word256 a3 a2 a1 a0) =
  popCount a3 + popCount a2 + popCount a1 + popCount a0

-- -----------------------------------------------------------------------------
-- Functions for `FiniteBits` instance.

countLeadingZeros256 :: Word256 -> Int
countLeadingZeros256 (Word256 a3 a2 a1 a0) =
  case countLeadingZeros a3 of
    64 -> case countLeadingZeros a2 of
      64 -> case countLeadingZeros a1 of
        64 -> 192 + countLeadingZeros a0
        res -> 128 + res
      res -> 64 + res
    res -> res

countTrailingZeros256 :: Word256 -> Int
countTrailingZeros256 (Word256 a3 a2 a1 a0) =
  case countTrailingZeros a0 of
    64 -> case countTrailingZeros a1 of
      64 -> case countTrailingZeros a2 of
        64 -> 192 + countTrailingZeros a3
        res -> 128 + res
      res -> 64 + res
    res -> res

-- -----------------------------------------------------------------------------
-- Functions for `Integral` instance.

-- TODO: This is inefficient, but the better version is rather
-- tedious to write out.
quotRem256 :: Word256 -> Word256 -> (Word256, Word256)
quotRem256 a b =
  let (x,y) = quotRem (toInteger256 a) (toInteger256 b)
   in (fromInteger256 x, fromInteger256 y)

toInteger256 :: Word256 -> Integer
toInteger256 (Word256 a3 a2 a1 a0) =
    (toInteger a3 `shiftL` 192)
  + (toInteger a2 `shiftL` 128)
  + (toInteger a1 `shiftL` 64)
  + (toInteger a0)

-- -----------------------------------------------------------------------------
-- Functions for `Storable` instance.

peek256 :: Ptr Word256 -> IO Word256
peek256 ptr = Word256
  <$> peekElemOff (castPtr ptr) index3
  <*> peekElemOff (castPtr ptr) index2
  <*> peekElemOff (castPtr ptr) index1
  <*> peekElemOff (castPtr ptr) index0

peekElemOff256 :: Ptr Word256 -> Int -> IO Word256
peekElemOff256 ptr idx = Word256
  <$> peekElemOff (castPtr ptr) (idx2 + index3)
  <*> peekElemOff (castPtr ptr) (idx2 + index2)
  <*> peekElemOff (castPtr ptr) (idx2 + index1)
  <*> peekElemOff (castPtr ptr) (idx2 + index0)
  where idx2 = 4 * idx

poke256 :: Ptr Word256 -> Word256 -> IO ()
poke256 ptr (Word256 a3 a2 a1 a0) = do
  pokeElemOff (castPtr ptr) index3 a3
  pokeElemOff (castPtr ptr) index2 a2
  pokeElemOff (castPtr ptr) index1 a1
  pokeElemOff (castPtr ptr) index0 a0

pokeElemOff256 :: Ptr Word256 -> Int -> Word256 -> IO ()
pokeElemOff256 ptr idx (Word256 a3 a2 a1 a0) = do
  pokeElemOff (castPtr ptr) (idx2 + index0) a0
  pokeElemOff (castPtr ptr) (idx2 + index1) a1
  pokeElemOff (castPtr ptr) (idx2 + index2) a2
  pokeElemOff (castPtr ptr) (idx2 + index3) a3
  where idx2 = 4 * idx

-- -----------------------------------------------------------------------------
-- Functions for `Prim` instance.

{-# INLINE sizeOf256# #-}
sizeOf256# :: Word256 -> Int#
sizeOf256# _ = 4# *# sizeOf# (undefined :: Word64)

{-# INLINE alignment256# #-}
alignment256# :: Word256 -> Int#
alignment256# _ = alignment# (undefined :: Word64)

{-# INLINE indexByteArray256# #-}
indexByteArray256# :: ByteArray# -> Int# -> Word256
indexByteArray256# arr# i# =
  let i2# = 4# *# i#
      w = indexByteArray# arr# (i2# +# unInt index3)
      x = indexByteArray# arr# (i2# +# unInt index2)
      y = indexByteArray# arr# (i2# +# unInt index1)
      z = indexByteArray# arr# (i2# +# unInt index0)
  in Word256 w x y z

{-# INLINE readByteArray256# #-}
readByteArray256# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word256 #)
readByteArray256# arr# i# =
  \s0 -> case readByteArray# arr# (i2# +# unInt index3) s0 of
    (# s1, w #) -> case readByteArray# arr# (i2# +# unInt index2) s1 of
      (# s2, x #) -> case readByteArray# arr# (i2# +# unInt index1) s2 of
        (# s3, y #) -> case readByteArray# arr# (i2# +# unInt index0) s3 of
          (# s4, z #) -> (# s4, Word256 w x y z #)
  where i2# = 4# *# i#

{-# INLINE writeByteArray256# #-}
writeByteArray256# :: MutableByteArray# s -> Int# -> Word256 -> State# s -> State# s
writeByteArray256# arr# i# (Word256 a b c d) =
  \s0 -> case writeByteArray# arr# (i2# +# unInt index3) a s0 of
    s1 -> case writeByteArray# arr# (i2# +# unInt index2) b s1 of
      s2 -> case writeByteArray# arr# (i2# +# unInt index1) c s2 of
        s3 -> case writeByteArray# arr# (i2# +# unInt index0) d s3 of
          s4 -> s4
  where i2# = 4# *# i#

{-# INLINE setByteArray256# #-}
setByteArray256# :: MutableByteArray# s -> Int# -> Int# -> Word256 -> State# s -> State# s
setByteArray256# = defaultSetByteArray#

{-# INLINE indexOffAddr256# #-}
indexOffAddr256# :: Addr# -> Int# -> Word256
indexOffAddr256# arr# i# =
  let i2# = 4# *# i#
      w = indexOffAddr# arr# (i2# +# unInt index3)
      x = indexOffAddr# arr# (i2# +# unInt index2)
      y = indexOffAddr# arr# (i2# +# unInt index1)
      z = indexOffAddr# arr# (i2# +# unInt index0)
  in Word256 w x y z

{-# INLINE readOffAddr256# #-}
readOffAddr256# :: Addr# -> Int# -> State# s -> (# State# s, Word256 #)
readOffAddr256# arr# i# =
  \s0 -> case readOffAddr# arr# (i2# +# unInt index3) s0 of
    (# s1, w #) -> case readOffAddr# arr# (i2# +# unInt index2) s1 of
      (# s2, x #) -> case readOffAddr# arr# (i2# +# unInt index1) s2 of
        (# s3, y #) -> case readOffAddr# arr# (i2# +# unInt index0) s3 of
          (# s4, z #) -> (# s4, Word256 w x y z #)
  where i2# = 4# *# i#

{-# INLINE writeOffAddr256# #-}
writeOffAddr256# :: Addr# -> Int# -> Word256 -> State# s -> State# s
writeOffAddr256# arr# i# (Word256 a b c d) =
  \s0 -> case writeOffAddr# arr# (i2# +# unInt index3) a s0 of
    s1 -> case writeOffAddr# arr# (i2# +# unInt index2) b s1 of
      s2 -> case writeOffAddr# arr# (i2# +# unInt index1) c s2 of
        s3 -> case writeOffAddr# arr# (i2# +# unInt index0) d s3 of
          s4 -> s4
  where i2# = 4# *# i#

{-# INLINE setOffAddr256# #-}
setOffAddr256# :: Addr# -> Int# -> Int# -> Word256 -> State# s -> State# s
setOffAddr256# = defaultSetOffAddr#

-- -----------------------------------------------------------------------------
-- Constants.

zeroWord256 :: Word256
zeroWord256 = Word256 0 0 0 0

oneWord256 :: Word256
oneWord256 = Word256 0 0 0 1

unInt :: Int -> Int#
unInt (I# i#) = i#

-- Use these indices to get the peek/poke ordering endian correct.
index0, index1, index2, index3 :: Int
#if WORDS_BIGENDIAN
index0 = 3
index1 = 2
index2 = 1
index3 = 0
#else
index0 = 0
index1 = 1
index2 = 2
index3 = 3
#endif
