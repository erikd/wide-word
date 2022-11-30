{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Data (Data, Typeable)
import Data.Ix (Ix)
#if ! MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

import GHC.Base (Int (..))
import GHC.Enum (predError, succError)
import GHC.Exts ((*#), (+#), Int#, State#, ByteArray#, MutableByteArray#, Addr#)
import GHC.Generics
import GHC.Real ((%), divZeroError)
import GHC.Word (Word64 (..), Word32, byteSwap64)

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Data.WideWord.Compat
import Numeric (showHex)

import Data.Primitive.Types (Prim (..), defaultSetByteArray#, defaultSetOffAddr#)

import Data.Hashable (Hashable,hashWithSalt)

data Word128 = Word128
  { word128Hi64 :: !Word64
  , word128Lo64 :: !Word64
  }
  deriving (Eq, Data, Generic, Ix, Typeable)

instance Hashable Word128 where
  hashWithSalt s (Word128 a1 a2) = s `hashWithSalt` a1 `hashWithSalt` a2

byteSwapWord128 :: Word128 -> Word128
byteSwapWord128 (Word128 a1 a0) = Word128 (byteSwap64 a0) (byteSwap64 a1)

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
  sizeOf w = I# (sizeOf128# w)
  alignment w = I# (alignment128# w)
  peek = peek128
  peekElemOff = peekElemOff128
  poke = poke128
  pokeElemOff = pokeElemOff128

instance NFData Word128 where
  -- The fields are already strict and unpacked, so do nothing.
  rnf !_ = ()

instance Prim Word128 where
  sizeOf#         = sizeOf128#
  alignment#      = alignment128#
  indexByteArray# = indexByteArray128#
  readByteArray#  = readByteArray128#
  writeByteArray# = writeByteArray128#
  setByteArray#   = setByteArray128#
  indexOffAddr#   = indexOffAddr128#
  readOffAddr#    = readOffAddr128#
  writeOffAddr#   = writeOffAddr128#
  setOffAddr#     = setOffAddr128#
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
"fromIntegral :: Word128 -> Word128" fromIntegral = id :: Word128 -> Word128

"fromIntegral :: Int -> Word128"     fromIntegral = fromInt
"fromIntegral :: Word -> Word128"    fromIntegral = fromWord
"fromIntegral :: Word32 -> Word128"  fromIntegral = fromWord32
"fromIntegral :: Word64 -> Word128"  fromIntegral = Word128 0

"fromIntegral :: Word128 -> Int"     fromIntegral = toInt
"fromIntegral :: Word128 -> Word"    fromIntegral = toWord
"fromIntegral :: Word128 -> Word32"  fromIntegral = toWord32
"fromIntegral :: Word128 -> Word64"  fromIntegral = \(Word128 _ w) -> w
  #-}

{-# INLINE fromInt #-}
fromInt :: Int -> Word128
fromInt = Word128 0 . fromIntegral

{-# INLINE fromWord #-}
fromWord :: Word -> Word128
fromWord = Word128 0 . fromIntegral

{-# INLINE fromWord32 #-}
fromWord32 :: Word32 -> Word128
fromWord32 = Word128 0 . fromIntegral

{-# INLINE toInt #-}
toInt :: Word128 -> Int
toInt (Word128 _ w) = fromIntegral w

{-# INLINE toWord #-}
toWord :: Word128 -> Word
toWord (Word128 _ w) = fromIntegral w

{-# INLINE toWord32 #-}
toWord32 :: Word128 -> Word32
toWord32 (Word128 _ w) = fromIntegral w

-- -----------------------------------------------------------------------------
-- Functions for `Ord` instance.

compare128 :: Word128 -> Word128 -> Ordering
compare128 (Word128 a1 a0) (Word128 b1 b0) =
  compare a1 b1 <> compare a0 b0

-- -----------------------------------------------------------------------------
-- Functions for `Enum` instance.

succ128 :: Word128 -> Word128
succ128 (Word128 a1 a0)
  | a0 == maxBound = if a1 == maxBound
                     then succError "Word128"
                     else Word128 (a1 + 1) 0
  | otherwise = Word128 a1 (a0 + 1)


pred128 :: Word128 -> Word128
pred128 (Word128 a1 a0)
  | a0 == 0 = if a1 == 0
              then predError "Word128"
              else Word128 (a1 - 1) maxBound
  | otherwise = Word128 a1 (a0 - 1)


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
  case plusWord2# (not# a0) (compatWordLiteral# 1##) of
    (# c, s #) -> Word128 (W64# (plusWord# (not# a1) c)) (W64# s)

{-# INLINABLE signum128 #-}
signum128 :: Word128 -> Word128
signum128 (Word128 (W64# a) (W64# b)) =
  if isZeroWord# a && isZeroWord# b
    then zeroWord128
    else oneWord128

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
  | n1 == d1 =
      case compare n0 d0 of
        LT -> (zeroWord128, num)
        EQ -> (oneWord128, zeroWord128)
        GT -> (Word128 0 1, Word128 0 (n0 - d0))
  | otherwise = quotRemFour num den

{-# INLINE quotRemFour #-}
quotRemFour :: Word128 -> Word128 -> (Word128, Word128)
quotRemFour num@(Word128 n1 _) den@(Word128 d1 _)
  | remain < den = (Word128 0 qest, remain)
    -- The above is correct in most cases, but for the case where is not
    -- we have the following. While the following is correct, it is rather
    -- suboptimal. Would be nice to find something better.
  | otherwise =
      mapPair fromInteger128 $ quotRem (toInteger num) (toInteger den)
  where
    qest = quot n1 d1
    prod = halfTimes128 den qest
    remain = minus128 num prod

{-# INLINE halfTimes128 #-}
halfTimes128 :: Word128 -> Word64 -> Word128
halfTimes128 (Word128 (W64# a1) (W64# a0)) (W64# b0) =
  Word128 (W64# p1) (W64# p0)
  where
    !(# c1, p0 #) = timesWord2# a0 b0
    p1a = timesWord# a1 b0
    p1 = plusWord# p1a c1

{-# INLINE quotRemThree #-}
quotRemThree :: Word128 -> Word64 -> (Word128, Word128)
quotRemThree num@(Word128 n1 n0) den
  | den == 0 = divZeroError
  | den == 1 = (num, zeroWord128)
  | n1 < den = case quotRemWord64 n1 n0 den of
                (q, r) -> (Word128 0 q, Word128 0 r)
  | otherwise =
      case quotRem n1 den of
        (q1, r1) -> case quotRemWord64 r1 n0 den of
                      (q0, r0) -> (Word128 q1 q0, Word128 0 r0)

{-# INLINE quotRemWord64 #-}
quotRemWord64 :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
quotRemWord64 (W64# n1) (W64# n0) (W64# d) =
  case quotRemWord2# n1 n0 d of
    (# q, r #) -> (W64# q, W64# r)

{-# INLINE quotRemTwo #-}
quotRemTwo :: Word64 -> Word64 -> (Word128, Word128)
quotRemTwo n0 d0 =
  case quotRem n0 d0 of
    (q, r) -> (Word128 0 q, Word128 0 r)

{-# INLINE toInteger128 #-}
toInteger128 :: Word128 -> Integer
toInteger128 (Word128 a1 a0) = fromIntegral a1 `shiftL` 64 + fromIntegral a0

-- -----------------------------------------------------------------------------
-- Functions for `Storable` instance.

peek128 :: Ptr Word128 -> IO Word128
peek128 ptr =
  Word128 <$> peekElemOff (castPtr ptr) index1 <*> peekElemOff (castPtr ptr) index0

peekElemOff128 :: Ptr Word128 -> Int -> IO Word128
peekElemOff128 ptr idx =
  Word128 <$> peekElemOff (castPtr ptr) (idx2 + index1)
            <*> peekElemOff (castPtr ptr) (idx2 + index0)
  where idx2 = 2 * idx

poke128 :: Ptr Word128 -> Word128 -> IO ()
poke128 ptr (Word128 a1 a0) =
  pokeElemOff (castPtr ptr) index1 a1 >> pokeElemOff (castPtr ptr) index0 a0

pokeElemOff128 :: Ptr Word128 -> Int -> Word128 -> IO ()
pokeElemOff128 ptr idx (Word128 a1 a0) = do
  let idx2 = 2 * idx
  pokeElemOff (castPtr ptr) (idx2 + index0) a0
  pokeElemOff (castPtr ptr) (idx2 + index1) a1

-- -----------------------------------------------------------------------------
-- Functions for `Prim` instance.

{-# INLINE sizeOf128# #-}
sizeOf128# :: Word128 -> Int#
sizeOf128# _ = 2# *# sizeOf# (0 :: Word64)

{-# INLINE alignment128# #-}
alignment128# :: Word128 -> Int#
alignment128# _ = 2# *# alignment# (0 :: Word64)

{-# INLINE indexByteArray128# #-}
indexByteArray128# :: ByteArray# -> Int# -> Word128
indexByteArray128# arr# i# =
  let i2# = 2# *# i#
      x = indexByteArray# arr# (i2# +# unInt index1)
      y = indexByteArray# arr# (i2# +# unInt index0)
  in Word128 x y

{-# INLINE readByteArray128# #-}
readByteArray128# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word128 #)
readByteArray128# arr# i# =
  \s0 -> case readByteArray# arr# (i2# +# unInt index1) s0 of
    (# s1, x #) -> case readByteArray# arr# (i2# +# unInt index0) s1 of
      (# s2, y #) -> (# s2, Word128 x y #)
  where i2# = 2# *# i#

{-# INLINE writeByteArray128# #-}
writeByteArray128# :: MutableByteArray# s -> Int# -> Word128 -> State# s -> State# s
writeByteArray128# arr# i# (Word128 a b) =
  \s0 -> case writeByteArray# arr# (i2# +# unInt index1) a s0 of
    s1 -> case writeByteArray# arr# (i2# +# unInt index0) b s1 of
      s2 -> s2
  where i2# = 2# *# i#

{-# INLINE setByteArray128# #-}
setByteArray128# :: MutableByteArray# s -> Int# -> Int# -> Word128 -> State# s -> State# s
setByteArray128# = defaultSetByteArray#

{-# INLINE indexOffAddr128# #-}
indexOffAddr128# :: Addr# -> Int# -> Word128
indexOffAddr128# addr# i# =
  let i2# = 2# *# i#
      x = indexOffAddr# addr# (i2# +# unInt index1)
      y = indexOffAddr# addr# (i2# +# unInt index0)
  in Word128 x y

{-# INLINE readOffAddr128# #-}
readOffAddr128# :: Addr# -> Int# -> State# s -> (# State# s, Word128 #)
readOffAddr128# addr# i# =
  \s0 -> case readOffAddr# addr# (i2# +# unInt index1) s0 of
    (# s1, x #) -> case readOffAddr# addr# (i2# +# unInt index0) s1 of
      (# s2, y #) -> (# s2, Word128 x y #)
  where i2# = 2# *# i#

{-# INLINE writeOffAddr128# #-}
writeOffAddr128# :: Addr# -> Int# -> Word128 -> State# s -> State# s
writeOffAddr128# addr# i# (Word128 a b) =
  \s0 -> case writeOffAddr# addr# (i2# +# unInt index1) a s0 of
    s1 -> case writeOffAddr# addr# (i2# +# unInt index0) b s1 of
      s2 -> s2
  where i2# = 2# *# i#

{-# INLINE setOffAddr128# #-}
setOffAddr128# :: Addr# -> Int# -> Int# -> Word128 -> State# s -> State# s
setOffAddr128# = defaultSetOffAddr#

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

-- -----------------------------------------------------------------------------
-- Constants.

zeroWord128 :: Word128
zeroWord128 = Word128 0 0

oneWord128 :: Word128
oneWord128 = Word128 0 1

unInt :: Int -> Int#
unInt (I# i#) = i#

-- Use these indices to get the peek/poke ordering endian correct.
index0, index1 :: Int
#if WORDS_BIGENDIAN
index0 = 1
index1 = 0
#else
index0 = 0
index1 = 1
#endif
