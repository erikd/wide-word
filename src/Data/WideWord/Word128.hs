{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  Data.WideWord.Word128
----
---- Maintainer  :  erikd@mega-nerd.com
---- Stability   :  experimental
---- Portability :  non-portable (GHC extensions and primops)
----
---- This module provides an opaque unsigned 128 bit value with the usual set
---- of typeclass instances one would expect for a fixed width unsigned integer
---- type.
---- Operations like addition, subtraction and multiplication etc provide a
---- "modulo 2^128" result as one would expect from a fixed width unsigned word.
-------------------------------------------------------------------------------

#include <MachDeps.h>

module Data.WideWord.Word128
  ( Word128 (..)
  , byteSwapWord128
  , showHexWord128
  , zeroWord128
  ) where

import Control.DeepSeq (NFData (..))

import Data.Bits (Bits (..), FiniteBits (..), shiftL)

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

import GHC.Base (Int (..), and#, int2Word#, minusWord#, not#, or#, plusWord#, plusWord2#
                , quotRemWord2#, subWordC#, timesWord#, timesWord2#, xor#)
import GHC.Enum (predError, succError)
import GHC.Real ((%), divZeroError)
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

instance Bits Word128 where
  (.&.) = and128
  (.|.) = or128
  xor = xor128
  complement = complement128
  shiftL = shiftL128
  unsafeShiftL = shiftL128
  shiftR = shiftR128
  unsafeShiftR = shiftR128
  rotateL = rotateL128
  rotateR = rotateR128

  bitSize _ = 128
  bitSizeMaybe _ = Just 128
  isSigned _ = False

  testBit = testBit128
  bit = bit128

  popCount = popCount128

instance FiniteBits Word128 where
  finiteBitSize _ = 128
  countLeadingZeros = countLeadingZeros128
  countTrailingZeros = countTrailingZeros128

instance Real Word128 where
  toRational x = toInteger128 x % 1

instance Integral Word128 where
  quot n d = fst (quotRem128 n d)
  rem n d = snd (quotRem128 n d)
  div n d = fst (quotRem128 n d)
  mod n d = snd (quotRem128 n d)
  quotRem = quotRem128
  divMod = quotRem128
  toInteger = toInteger128

instance Storable Word128 where
  sizeOf _ = 2 * sizeOf (0 :: Word64)
  alignment _ = 2 * alignment (0 :: Word64)
  peek = peek128
  peekElemOff = peekElemOff128
  poke = poke128
  pokeElemOff = pokeElemOff128

instance NFData Word128 where
  rnf (Word128 a1 a0) = rnf a1 `seq` rnf a0

-- -----------------------------------------------------------------------------
-- Rewrite rules.

{-# RULES
"fromIntegral :: Word128 -> Word128" fromIntegral = id :: Word128 -> Word128
  #-}

{-# RULES
"fromIntegral :: Int -> Word128"     fromIntegral = \(I# i#) -> Word128 (W64# 0##) (W64# (int2Word# i#))
"fromIntegral :: Word- > Word128"    fromIntegral = Word128 0 . fromIntegral
"fromIntegral :: Word32 -> Word128"  fromIntegral = Word128 0 . fromIntegral
"fromIntegral :: Word64 -> Word128"  fromIntegral = Word128 0

"fromIntegral :: Word128 -> Int"     fromIntegral = \(Word128 _ w) -> fromIntegral w
"fromIntegral :: Word128 -> Word"    fromIntegral = \(Word128 _ w) -> fromIntegral w
"fromIntegral :: Word128 -> Word32"  fromIntegral = \(Word128 _ w) -> fromIntegral w
"fromIntegral :: Word128 -> Word64"  fromIntegral = \(Word128 _ w) -> w
  #-}

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

succ128 :: Word128 -> Word128
succ128 (Word128 a1 a0)
  | a1 == maxBound && a0 == maxBound = succError "Word128"
  | otherwise =
      case a0 + 1 of
        0 -> Word128 (a1 + 1) 0
        s -> Word128 a1 s

pred128 :: Word128 -> Word128
pred128 (Word128 a1 a0)
  | a1 == 0 && a0 == 0 = predError "Word128"
  | otherwise =
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
    !(# c1, s0 #) = plusWord2# a0 b0
    s1a = plusWord# a1 b1
    s1 = plusWord# c1 s1a

{-# INLINABLE minus128 #-}
minus128 :: Word128 -> Word128 -> Word128
minus128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# d1) (W64# d0)
  where
    !(# d0, c1 #) = subWordC# a0 b0
    a1c = minusWord# a1 (int2Word# c1)
    d1 = minusWord# a1c b1

times128 :: Word128 -> Word128 -> Word128
times128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# p1) (W64# p0)
  where
    !(# c1, p0 #) = timesWord2# a0 b0
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
-- Functions for `Bits` instance.

{-# INLINABLE and128 #-}
and128 :: Word128 -> Word128 -> Word128
and128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# (and# a1 b1)) (W64# (and# a0 b0))

{-# INLINABLE or128 #-}
or128 :: Word128 -> Word128 -> Word128
or128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# (or# a1 b1)) (W64# (or# a0 b0))

{-# INLINABLE xor128 #-}
xor128 :: Word128 -> Word128 -> Word128
xor128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# (xor# a1 b1)) (W64# (xor# a0 b0))

{-# INLINABLE complement128 #-}
complement128 :: Word128 -> Word128
complement128 (Word128 a1 a0) = Word128 (complement a1) (complement a0)

-- Probably not worth inlining this.
shiftL128 :: Word128 -> Int -> Word128
shiftL128 w@(Word128 a1 a0) s
  | s == 0 = w
  | s < 0 = shiftL128 w (128 - (abs s `mod` 128))
  | s >= 128 = zeroWord128
  | s == 64 = Word128 a0 0
  | s > 64 = Word128 (a0 `shiftL` (s - 64)) 0
  | otherwise =
      Word128 s1 s0
      where
        s0 = a0 `shiftL` s
        s1 = a1 `shiftL` s + a0 `shiftR` (64 - s)

-- Probably not worth inlining this.
shiftR128 :: Word128 -> Int -> Word128
shiftR128 w@(Word128 a1 a0) s
  | s < 0 = zeroWord128
  | s == 0 = w
  | s >= 128 = zeroWord128
  | s == 64 = Word128 0 a1
  | s > 64 = Word128 0 (a1 `shiftR` (s - 64))
  | otherwise =
      Word128 s1 s0
      where
        s1 = a1 `shiftR` s
        s0 = a0 `shiftR` s + a1 `shiftL` (64 - s)

rotateL128 :: Word128 -> Int -> Word128
rotateL128 w@(Word128 a1 a0) r
  | r < 0 = zeroWord128
  | r == 0 = w
  | r >= 128 = rotateL128 w (r `mod` 128)
  | r == 64 = Word128 a0 a1
  | r > 64 = rotateL128 (Word128 a0 a1) (r `mod` 64)
  | otherwise =
      Word128 s1 s0
      where
        s0 = a0 `shiftL` r + a1 `shiftR` (64 - r)
        s1 = a1 `shiftL` r + a0 `shiftR` (64 - r)

rotateR128 :: Word128 -> Int -> Word128
rotateR128 w@(Word128 a1 a0) r
  | r < 0 = rotateR128 w (128 - (abs r `mod` 128))
  | r == 0 = w
  | r >= 128 = rotateR128 w (r `mod` 128)
  | r == 64 = Word128 a0 a1
  | r > 64 = rotateR128 (Word128 a0 a1) (r `mod` 64)
  | otherwise =
      Word128 s1 s0
      where
        s0 = a0 `shiftR` r + a1 `shiftL` (64 - r)
        s1 = a1 `shiftR` r + a0 `shiftL` (64 - r)

testBit128 :: Word128 -> Int -> Bool
testBit128 (Word128 a1 a0) i
  | i < 0 = False
  | i >= 128 = False
  | i >= 64 = testBit a1 (i - 64)
  | otherwise = testBit a0 i

bit128 :: Int -> Word128
bit128 indx
  | indx < 0 = zeroWord128
  | indx >= 128 = zeroWord128
  | otherwise = shiftL128 oneWord128 indx

popCount128 :: Word128 -> Int
popCount128 (Word128 a1 a0) = popCount a1 + popCount a0

-- -----------------------------------------------------------------------------
-- Functions for `FiniteBits` instance.

countLeadingZeros128 :: Word128 -> Int
countLeadingZeros128 (Word128 a1 a0) =
  case countLeadingZeros a1 of
    64 -> 64 +  countLeadingZeros a0
    res -> res

countTrailingZeros128 :: Word128 -> Int
countTrailingZeros128 (Word128 a1 a0) =
  case countTrailingZeros a0 of
    64 -> 64 + countTrailingZeros a1
    res -> res

-- -----------------------------------------------------------------------------
-- Functions for `Integral` instance.

quotRem128 :: Word128 -> Word128 -> (Word128, Word128)
quotRem128 num@(Word128 n1 n0) den@(Word128 d1 d0)
  | n1 == 0 && d1 == 0 = quotRemTwo n0 d0
  | n1 < d1 = (zeroWord128, num)
  | d1 == 0 = quotRemThree num d0
  | otherwise = quotRemFour num den


quotRemFour :: Word128 -> Word128 -> (Word128, Word128)
quotRemFour num@(Word128 n1 _) den@(Word128 d1 d0)
  | n1 == d1 = quotRemFourX num d0
  | otherwise = (q, r)
      where
        qtest = quot n1 d1
        diff = times128 den (Word128 0 qtest)
        (q, r) = case compare128 num diff of
                    EQ -> (Word128 0 qtest, zeroWord128)
                    GT -> (Word128 0 qtest, minus128 num diff)
                    LT -> let qx = Word128 0 (qtest - 1)
                              diffx = times128 den qx
                          in (qx, minus128 num diffx)


{-# INLINE quotRemFourX #-}
quotRemFourX :: Word128 -> Word64 -> (Word128, Word128)
quotRemFourX num@(Word128 _ n0) d0 =
  case compare n0 d0 of
    LT -> (zeroWord128, num)
    EQ -> (oneWord128, zeroWord128)
    GT -> (Word128 0 1, Word128 0 (n0 - d0))


{-# INLINE quotRemThree #-}
quotRemThree :: Word128 -> Word64 -> (Word128, Word128)
quotRemThree num@(Word128 n1 n0) den
  | den == 0 = divZeroError
  | den == 1 = (num, zeroWord128)
  | n1 < den = case quotRemWord2 n1 n0 den of
                (q, r) -> (Word128 0 q, Word128 0 r)
  | otherwise =
      case quotRem n1 den of
        (q1, r1) -> case quotRemWord2 r1 n0 den of
                      (q0, r0) -> (Word128 q1 q0, Word128 0 r0)

{-# INLINE quotRemWord2 #-}
quotRemWord2 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
quotRemWord2 (W64# n1) (W64# n0) (W64# d) =
  case quotRemWord2# n1 n0 d of
    (# q, r #) -> (W64# q, W64# r)


{-# INLINE quotRemTwo #-}
quotRemTwo :: Word64 -> Word64 -> (Word128, Word128)
quotRemTwo n0 d0 =
  case quotRem n0 d0 of
    (q, r) -> (Word128 0 q, Word128 0 r)

toInteger128 :: Word128 -> Integer
toInteger128 (Word128 a1 a0) = fromIntegral a1 `shiftL` 64 + fromIntegral a0

-- -----------------------------------------------------------------------------
-- Functions for `Integral` instance.

peek128 :: Ptr Word128 -> IO Word128
peek128 ptr =
  Word128 <$> peekElemOff (castPtr ptr) index1 <*> peekElemOff (castPtr ptr) index0

peekElemOff128 :: Ptr Word128 -> Int -> IO Word128
peekElemOff128 ptr idx =
  Word128 <$> peekElemOff (castPtr ptr) (2 * idx + index1)
            <*> peekElemOff (castPtr ptr) (2 * idx + index0)

poke128 :: Ptr Word128 -> Word128 -> IO ()
poke128 ptr (Word128 a1 a0) =
  pokeElemOff (castPtr ptr) index1 a1 >> pokeElemOff (castPtr ptr) index0 a0

pokeElemOff128 :: Ptr Word128 -> Int -> Word128 -> IO ()
pokeElemOff128 ptr idx (Word128 a1 a0) = do
  pokeElemOff (castPtr ptr) (2 * idx + index0) a0
  pokeElemOff (castPtr ptr) (2 * idx + index1) a1

-- -----------------------------------------------------------------------------
-- Constants.

zeroWord128 :: Word128
zeroWord128 = Word128 0 0

oneWord128 :: Word128
oneWord128 = Word128 0 1

-- Use these indices to get the peek/poke ordering endian correct.
index0, index1 :: Int
#if WORDS_BIGENDIAN
index0 = 1
index1 = 0
#else
index0 = 0
index1 = 1
#endif
