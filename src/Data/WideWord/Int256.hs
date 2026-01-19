{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.WideWord.Int256
  ( Int256 (..)
  , byteSwapInt256
  , showHexInt256
  , zeroInt256
  ) where

import Control.DeepSeq (NFData (..))

import Data.Bits (Bits (..), FiniteBits (..), shiftL)
import Data.Data (Data)
import Data.Ix (Ix)
#if ! MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif


import Numeric

import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

import GHC.Base (Int (..))
import GHC.Enum (predError, succError)
import GHC.Exts ((*#), (+#), Int#, State#, Addr#, ByteArray#, MutableByteArray#)
import GHC.Generics
import GHC.Real ((%))
import GHC.Word (Word32, Word64, byteSwap64)

import Data.Primitive.Types (Prim (..), defaultSetByteArray#, defaultSetOffAddr#)

import Data.Hashable (Hashable, hashWithSalt)
import Data.Binary (Binary (get, put))

import Data.WideWord.Word64


data Int256 = Int256
  { int256hi :: !Word64
  , int256m1 :: !Word64
  , int256m0 :: !Word64
  , int256lo :: !Word64
  }
  deriving (Eq, Data, Generic, Ix)

instance Hashable Int256 where
  hashWithSalt s (Int256 a1 a2 a3 a4) =
    s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3 `hashWithSalt` a4

-- | @since 0.1.5.0
instance Binary Int256 where
  put (Int256 a1 a2 a3 a4) = put a1 >> put a2 >> put a3 >> put a4
  get = Int256 <$> get <*> get <*> get <*> get

byteSwapInt256 :: Int256 -> Int256
byteSwapInt256 (Int256 a3 a2 a1 a0) = Int256 (byteSwap64 a0) (byteSwap64 a1) (byteSwap64 a2) (byteSwap64 a3)

showHexInt256 :: Int256 -> String
showHexInt256 (Int256 a3 a2 a1 a0)
  | a3 == 0 =
      if a2 == 0
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

instance Show Int256 where
  show = show . toInteger

instance Read Int256 where
  readsPrec p s = [(fromInteger256 (x :: Integer), r) | (x, r) <- readsPrec p s]

instance Ord Int256 where
  compare = compare256

instance Bounded Int256 where
  minBound = Int256 0x8000000000000000 0 0 0
  maxBound = Int256 0x7fffffffffffffff maxBound maxBound maxBound

instance Enum Int256 where
  succ = succ256
  pred = pred256
  toEnum = toEnum256
  fromEnum = fromEnum256

instance Num Int256 where
  (+) = plus256
  (-) = minus256
  (*) = times256
  negate = negate256
  abs = abs256
  signum = signum256
  fromInteger = fromInteger256

instance Bits Int256 where
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
  isSigned _ = True

  testBit = testBit256
  bit = bit256

  popCount = popCount256

instance FiniteBits Int256 where
  finiteBitSize _ = 256
  countLeadingZeros = countLeadingZeros256
  countTrailingZeros = countTrailingZeros256

instance Real Int256 where
  toRational x = toInteger256 x % 1

instance Integral Int256 where
  quot n d = fst (quotRem256 n d)
  rem n d = snd (quotRem256 n d)
  div n d = fst (divMod256 n d)
  mod n d = snd (divMod256 n d)
  quotRem = quotRem256
  divMod = divMod256
  toInteger = toInteger256


instance Storable Int256 where
  sizeOf i = I# (sizeOf256# i)
  alignment i = I# (alignment256# i)
  peek = peek256
  peekElemOff = peekElemOff256
  poke = poke256
  pokeElemOff = pokeElemOff256

instance NFData Int256 where
  -- The fields are already strict and unpacked, so do nothing.
  rnf !_ = ()

instance Prim Int256 where
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
"fromIntegral :: Int -> Int256"     fromIntegral = fromInt
"fromIntegral :: Word -> Int256"    fromIntegral = fromWord
"fromIntegral :: Word32 -> Int256"  fromIntegral = fromWord32
"fromIntegral :: Word64 -> Int256"  fromIntegral = Int256 0 0 0

"fromIntegral :: Int256 -> Int"     fromIntegral = toInt
"fromIntegral :: Int256 -> Word"    fromIntegral = toWord
"fromIntegral :: Int256 -> Word32"  fromIntegral = toWord32
"fromIntegral :: Int256 -> Word64"  fromIntegral = \(Int256 _ _ _ w) -> w
  #-}

{-# INLINE fromInt #-}
fromInt :: Int -> Int256
fromInt = Int256 0 0 0 . fromIntegral

{-# INLINE fromWord #-}
fromWord :: Word -> Int256
fromWord = Int256 0 0  0 . fromIntegral

{-# INLINE fromWord32 #-}
fromWord32 :: Word32 -> Int256
fromWord32 = Int256 0 0 0 . fromIntegral

{-# INLINE toInt #-}
toInt :: Int256 -> Int
toInt (Int256 _ _ _ w) = fromIntegral w

{-# INLINE toWord #-}
toWord :: Int256 -> Word
toWord (Int256 _ _ _ w) = fromIntegral w

{-# INLINE toWord32 #-}
toWord32 :: Int256 -> Word32
toWord32 (Int256 _ _ _ w) = fromIntegral w

-- -----------------------------------------------------------------------------
-- Functions for `Ord` instance.

compare256 :: Int256 -> Int256 -> Ordering
compare256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0)
  | aIsNeg == bIsNeg = compare a3 b3 <> compare a2 b2 <> compare a1 b1 <> compare a0 b0
  | bIsNeg = GT
  | otherwise = LT
  where
    aIsNeg = isNeg a3
    bIsNeg = isNeg b3
    isNeg = (>= 0x8000000000000000)

-- -----------------------------------------------------------------------------
-- Functions for `Enum` instance.

succ256 :: Int256 -> Int256
succ256 (Int256 a3 a2 a1 a0)
  | a0 == maxBound =
      if a1 == maxBound
        then if a2 == maxBound
          then if a3 == 0x7fffffffffffffff
            then succError "Int256"
            else Int256 (a3 + 1) 0 0 0
          else Int256 a3 (a2 + 1) 0 0
        else Int256 a3 a2 (a1 + 1) 0
  | otherwise = Int256 a3 a2 a1 (a0 + 1)


pred256 :: Int256 -> Int256
pred256 (Int256 a3 a2 a1 a0)
  | a0 == 0 =
      if a1 == 0
        then if a2 == 0
          then if a3 == 0x8000000000000000
            then predError "Int256"
            else Int256 (a3 - 1) maxBound maxBound maxBound
          else Int256 a3 (a2 - 1) maxBound maxBound
        else Int256 a3 a2 (a1 - 1) maxBound
  | otherwise = Int256 a3 a2 a1 (a0 - 1)



{-# INLINABLE toEnum256 #-}
toEnum256 :: Int -> Int256
toEnum256 i = Int256 0 0 0 (toEnum i)

{-# INLINABLE fromEnum256 #-}
fromEnum256 :: Int256 -> Int
fromEnum256 (Int256 _ _ _ a0) = fromEnum a0

-- -----------------------------------------------------------------------------
-- Functions for `Num` instance.

{-# INLINABLE plus256 #-}
plus256 :: Int256 -> Int256 -> Int256
plus256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0) =
    Int256 s3 s2 s1 s0
  where
    !(c1, s0) = plusCarrySum a0 b0
    !(c2a, s1a) = plusCarrySum a1 b1
    !(c2b, s1) = plusCarrySum s1a c1
    !c2 = c2a + c2b
    !(c3a, s2a) = plusCarrySum a2 b2
    !(c3b, s2) = plusCarrySum s2a c2
    !c3 = c3a + c3b
    !s3 = a3 + b3 + c3

{-# INLINABLE minus256 #-}
minus256 :: Int256 -> Int256 -> Int256
minus256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0) =
    Int256 d3 d2 d1 d0
  where
    !(c1, d0) = subCarryDiff a0 b0
    !(c2a, b1a) = plusCarrySum b1 c1
    !(c2b, d1) = subCarryDiff a1 b1a
    !c2 = c2a + c2b
    !(c3a, b2a) = plusCarrySum b2 c2
    !(c3b, d2) = subCarryDiff a2 b2a
    !c3 = c3a + c3b
    !d3 = a3 - b3 - c3

times256 :: Int256 -> Int256 -> Int256
times256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0) =
  Int256 r3 r2 r1 r0
  where
    !(c00, p00) = timesCarryProd a0 b0
    !(c01, p01) = timesCarryProd a0 b1
    !(c02, p02) = timesCarryProd a0 b2
    !p03 = a0 * b3
    !(c10, p10) = timesCarryProd a1 b0
    !(c11, p11) = timesCarryProd a1 b1
    !p12 = a1 * b2
    !(c20, p20) = timesCarryProd a2 b0
    !p21 = a2 * b1
    !p30 = a3 * b0
    !r0 = p00
    !c1 = c00
    !(c2x, r1a) = plusCarrySum p01 p10
    !(c2y, r1b) = plusCarrySum r1a c1
    !(c3w, c2) = plusCarrySum c2x c2y
    !r1 = r1b
    !(c3x, r2a) = plusCarrySum p11 p20
    !(c3y, r2b) = plusCarrySum p02 r2a
    !(c3z, r2c) = plusCarrySum r2b c2
    !(c3s, r2d) = plusCarrySum r2c c01
    !(c3t, r2e) = plusCarrySum r2d c10
    !r2 = r2e
    !r3 = p30 + p21 + p12 + p03 + c3w + c3x +
           c3y + c3z + c3s + c3t + c02 + c11 + c20

{-# INLINABLE negate256 #-}
negate256 :: Int256 -> Int256
negate256 (Int256 a3 a2 a1 a0) =
  case plusCarrySum (complement a0) 1 of
    (c1, s0) -> case plusCarrySum (complement a1) c1 of
      (c2, s1) -> case plusCarrySum (complement a2) c2 of
        (c3, s2) -> case complement a3 + c3 of
          s3 -> Int256 s3 s2 s1 s0

{-# INLINABLE abs256 #-}
abs256 :: Int256 -> Int256
abs256 i@(Int256 a3 _ _ _)
  | testBit a3 63 = negate256 i
  | otherwise = i

{-# INLINABLE complement256 #-}
complement256 :: Int256 -> Int256
complement256 (Int256 a3 a2 a1 a0) = Int256 (complement a3) (complement a2) (complement a1) (complement a0)


{-# INLINABLE signum256 #-}
signum256 :: Int256 -> Int256
signum256 (Int256 a3 a2 a1 a0)
  | a3 == 0 && a2 == 0 && a1 == 0 && a0 == 0 = zeroInt256
  | testBit a3 63 = minusOneInt256
  | otherwise = oneInt256
fromInteger256 :: Integer -> Int256
fromInteger256 i =
  Int256
    (fromIntegral $ i `shiftR` 192) (fromIntegral $ i `shiftR` 128)
    (fromIntegral $ i `shiftR` 64) (fromIntegral i)

-- -----------------------------------------------------------------------------
-- Functions for `Bits` instance.

{-# INLINABLE and256 #-}
and256 :: Int256 -> Int256 -> Int256
and256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0) =
  Int256 (a3 .&. b3) (a2 .&. b2) (a1 .&. b1) (a0 .&. b0)

{-# INLINABLE or256 #-}
or256 :: Int256 -> Int256 -> Int256
or256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0) =
  Int256 (a3 .|. b3) (a2 .|. b2) (a1 .|. b1) (a0 .|. b0)

{-# INLINABLE xor256 #-}
xor256 :: Int256 -> Int256 -> Int256
xor256 (Int256 a3 a2 a1 a0) (Int256 b3 b2 b1 b0) =
  Int256 (xor a3 b3) (xor a2 b2) (xor a1 b1) (xor a0 b0)

-- Some of the following functions have quite complicated guard clauses, but we make them
-- inlineable anyway so that if the things like the shift amount is a compile time constant
-- most of the function can be dropped leaving only the needed bits inlined.

{-# INLINABLE shiftL256 #-}
shiftL256 :: Int256 -> Int -> Int256
shiftL256 w@(Int256 a3 a2 a1 a0) s
  | s == 0 = w
  | s == minBound = zeroInt256
  | s < 0 = shiftR256 w (negate s)
  | s >= 256 = zeroInt256
  | s > 192 = Int256 (a0 `shiftL` (s - 192)) 0 0 0
  | s == 192 = Int256 a0 0 0 0
  | s > 128 =
      Int256
        (a1 `shiftL` (s - 128) + a0 `shiftR` (192 - s))
        (a0 `shiftL` (s - 128)) 0 0
  | s == 128 = Int256 a1 a0 0 0
  | s > 64 =
      Int256
        (a2 `shiftL` (s - 64) + a1 `shiftR` (128 - s))
        (a1 `shiftL` (s - 64) + a0 `shiftR` (128 - s))
        (a0 `shiftL` (s - 64))
        0
  | s == 64 = Int256 a2 a1 a0 0
  | otherwise =
      Int256
        (a3 `shiftL` s + a2 `shiftR` (64 - s))
        (a2 `shiftL` s + a1 `shiftR` (64 - s))
        (a1 `shiftL` s + a0 `shiftR` (64 - s))
        (a0 `shiftL` s)

{-# INLINABLE shiftR256 #-}
shiftR256 :: Int256 -> Int -> Int256
shiftR256 i@(Int256 a3 a2 a1 a0) s
  | s == 0 = i
  | s == minBound = zeroInt256
  | s < 0 = shiftL256 i (negate s)
  | topBitSetWord64 a3 = complement256 (shiftR256 (complement256 i) s)
  | s >= 256 = zeroInt256
  | s > 192 = Int256 0 0 0 (a3 `shiftR` (s - 192))
  | s == 192 = Int256 0 0 0 a3
  | s > 128 =
      Int256 0 0 (a3 `shiftR` (s - 128)) (a2 `shiftR` (s - 128) + a3 `shiftL` (192 - s))
  | s == 128 = Int256 0 0 a3 a2
  | s > 64 =
      Int256 0 (a3 `shiftR` (s - 64))
        (a2 `shiftR` (s - 64) + a3 `shiftL` (128 - s))
        (a1 `shiftR` (s - 64) + a2 `shiftL` (128 - s))
  | s == 64 = Int256 0 a3 a2 a1
  | otherwise =
      Int256
        (a3 `shiftR` s)
        (a2 `shiftR` s + a3 `shiftL` (64 - s))
        (a1 `shiftR` s + a2 `shiftL` (64 - s))
        (a0 `shiftR` s + a1 `shiftL` (64 - s))

{-# INLINABLE rotateL256 #-}
rotateL256 :: Int256 -> Int -> Int256
rotateL256 w@(Int256 a3 a2 a1 a0) r
  | r < 0 = rotateR256 w ((abs r) `mod` 256)
  | r == 0 = w
  | r >= 256 = rotateL256 w (r `mod` 256)
  | r >= 192 = rotateL256 (Int256 a0 a3 a2 a1) (r - 192)
  | r >= 128 = rotateL256 (Int256 a1 a0 a3 a2) (r - 128)
  | r >= 64 = rotateL256 (Int256 a2 a1 a0 a3) (r - 64)
  | otherwise =
      Int256
        (a3 `shiftL` r + a2 `shiftR` (64 - r))
        (a2 `shiftL` r + a1 `shiftR` (64 - r))
        (a1 `shiftL` r + a0 `shiftR` (64 - r))
        (a0 `shiftL` r + a3 `shiftR` (64 - r))

{-# INLINABLE rotateR256 #-}
rotateR256 :: Int256 -> Int -> Int256
rotateR256 w@(Int256 a3 a2 a1 a0) r
  | r < 0 = rotateL256 w ((abs r) `mod` 256)
  | r == 0 = w
  | r >= 256 = rotateR256 w (r `mod` 256)
  | r >= 192 = rotateR256 (Int256 a2 a1 a0 a3) (r - 192)
  | r >= 128 = rotateR256 (Int256 a1 a0 a3 a2) (r - 128)
  | r >= 64 = rotateR256 (Int256 a0 a3 a2 a1) (r - 64)
  | otherwise =
      Int256
        (a3 `shiftR` r + a0 `shiftL` (64 - r)) (a2 `shiftR` r + a3 `shiftL` (64 - r))
        (a1 `shiftR` r + a2 `shiftL` (64 - r)) (a0 `shiftR` r + a1 `shiftL` (64 - r))

{-# INLINABLE testBit256 #-}
testBit256 :: Int256 -> Int -> Bool
testBit256 (Int256 a3 a2 a1 a0) i
  | i < 0 = False
  | i >= 256 = False
  | i >= 192 = testBit a3 (i - 192)
  | i >= 128 = testBit a2 (i - 128)
  | i >= 64 = testBit a1 (i - 64)
  | otherwise = testBit a0 i

{-# INLINABLE bit256 #-}
bit256 :: Int -> Int256
bit256 indx
  | indx < 0 = zeroInt256
  | indx >= 256 = zeroInt256
  | otherwise = shiftL256 oneInt256 indx

{-# INLINABLE popCount256 #-}
popCount256 :: Int256 -> Int
popCount256 (Int256 a3 a2 a1 a0) =
  popCount a3 + popCount a2 + popCount a1 + popCount a0


-- -----------------------------------------------------------------------------
-- Functions for `FiniteBits` instance.

{-# INLINABLE countLeadingZeros256 #-}
countLeadingZeros256 :: Int256 -> Int
countLeadingZeros256 (Int256 a3 a2 a1 a0) =
  case countLeadingZeros a3 of
    64 -> case countLeadingZeros a2 of
      64 -> case countLeadingZeros a1 of
        64 -> 192 + countLeadingZeros a0
        res -> 128 + res
      res -> 64 + res
    res -> res

{-# INLINABLE countTrailingZeros256 #-}
countTrailingZeros256 :: Int256 -> Int
countTrailingZeros256 (Int256 a3 a2 a1 a0) =
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
quotRem256 :: Int256 -> Int256 -> (Int256, Int256)
quotRem256 a b =
  let (x,y) = quotRem (toInteger256 a) (toInteger256 b)
   in (fromInteger256 x, fromInteger256 y)

divMod256 :: Int256 -> Int256 -> (Int256, Int256)
divMod256 a b = let (x,y) = divMod (toInteger256 a) (toInteger256 b)
   in (fromInteger256 x, fromInteger256 y)

toInteger256 :: Int256 -> Integer
toInteger256 i@(Int256 a3 a2 a1 a0)
  | popCount a3 == 64 && popCount a2 == 64 && popCount a1 == 64 && popCount a0 == 64 = -1
  | not (testBit a3 63) =
    (fromIntegral a3 `shiftL` 192)
    + (fromIntegral a2 `shiftL` 128)
    + (fromIntegral a1 `shiftL` 64)
    + fromIntegral a0
  | otherwise =
      case negate256 i of
        Int256 n3 n2 n1 n0 -> negate $
          (fromIntegral n3 `shiftL` 192)
          + (fromIntegral n2 `shiftL` 128)
          + (fromIntegral n1 `shiftL` 64)
          + fromIntegral n0

-- -----------------------------------------------------------------------------
-- Functions for `Storable` instance.

peek256 :: Ptr Int256 -> IO Int256
peek256 ptr =
  Int256 <$> peekElemOff (castPtr ptr) index3 <*> peekElemOff (castPtr ptr) index2
    <*> peekElemOff (castPtr ptr) index1 <*> peekElemOff (castPtr ptr) index0

peekElemOff256 :: Ptr Int256 -> Int -> IO Int256
peekElemOff256 ptr idx =
  Int256 <$> peekElemOff (castPtr ptr) (idx2 + index3)
    <*> peekElemOff (castPtr ptr) (idx2 + index2)
    <*> peekElemOff (castPtr ptr) (idx2 + index1)
    <*> peekElemOff (castPtr ptr) (idx2 + index0)
  where
    idx2 = 4 * idx

poke256 :: Ptr Int256 -> Int256 -> IO ()
poke256 ptr (Int256 a3 a2 a1 a0) = do
  pokeElemOff (castPtr ptr) index3 a3
  pokeElemOff (castPtr ptr) index2 a2
  pokeElemOff (castPtr ptr) index1 a1
  pokeElemOff (castPtr ptr) index0 a0

pokeElemOff256 :: Ptr Int256 -> Int -> Int256 -> IO ()
pokeElemOff256 ptr idx (Int256 a3 a2 a1 a0) = do
    pokeElemOff (castPtr ptr) (idx2 + index0) a0
    pokeElemOff (castPtr ptr) (idx2 + index1) a1
    pokeElemOff (castPtr ptr) (idx2 + index2) a2
    pokeElemOff (castPtr ptr) (idx2 + index3) a3
  where
    idx2 = 4 * idx

-- -----------------------------------------------------------------------------
-- Helpers.

{-# INLINE topBitSetWord64 #-}
topBitSetWord64 :: Word64 -> Bool
topBitSetWord64 w = testBit w 63

-- -----------------------------------------------------------------------------
-- Functions for `Prim` instance.

{-# INLINE sizeOf256# #-}
sizeOf256# :: Int256 -> Int#
sizeOf256# _ = 4# *# sizeOf# (0 :: Word64)

{-# INLINE alignment256# #-}
alignment256# :: Int256 -> Int#
alignment256# _ = 4# *# alignment# (0 :: Word64)

{-# INLINE indexByteArray256# #-}
indexByteArray256# :: ByteArray# -> Int# -> Int256
indexByteArray256# arr# i# =
  let i2# = 4# *# i#
      w = indexByteArray# arr# (i2# +# unInt index3)
      x = indexByteArray# arr# (i2# +# unInt index2)
      y = indexByteArray# arr# (i2# +# unInt index1)
      z = indexByteArray# arr# (i2# +# unInt index0)
  in Int256 w x y z

{-# INLINE readByteArray256# #-}
readByteArray256# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int256 #)
readByteArray256# arr# i# =
  \s0 -> case readByteArray# arr# (i2# +# unInt index3) s0 of
    (# s1, w #) -> case readByteArray# arr# (i2# +# unInt index2) s1 of
      (# s2, x #) -> case readByteArray# arr# (i2# +# unInt index1) s2 of
        (# s3, y #) -> case readByteArray# arr# (i2# +# unInt index0) s3 of
          (# s4, z #) -> (# s4, Int256 w x y z #)
  where i2# = 4# *# i#

{-# INLINE writeByteArray256# #-}
writeByteArray256# :: MutableByteArray# s -> Int# -> Int256 -> State# s -> State# s
writeByteArray256# arr# i# (Int256 a b c d) =
  \s0 -> case writeByteArray# arr# (i2# +# unInt index3) a s0 of
    s1 -> case writeByteArray# arr# (i2# +# unInt index2) b s1 of
      s2 -> case writeByteArray# arr# (i2# +# unInt index1) c s2 of
        s3 -> case writeByteArray# arr# (i2# +# unInt index0) d s3 of
          s4 -> s4
  where i2# = 4# *# i#

{-# INLINE setByteArray256# #-}
setByteArray256# :: MutableByteArray# s -> Int# -> Int# -> Int256 -> State# s -> State# s
setByteArray256# = defaultSetByteArray#

{-# INLINE indexOffAddr256# #-}
indexOffAddr256# :: Addr# -> Int# -> Int256
indexOffAddr256# arr# i# =
  let i2# = 4# *# i#
      w = indexOffAddr# arr# (i2# +# unInt index3)
      x = indexOffAddr# arr# (i2# +# unInt index2)
      y = indexOffAddr# arr# (i2# +# unInt index1)
      z = indexOffAddr# arr# (i2# +# unInt index0)
  in Int256 w x y z

{-# INLINE readOffAddr256# #-}
readOffAddr256# :: Addr# -> Int# -> State# s -> (# State# s, Int256 #)
readOffAddr256# arr# i# =
  \s0 -> case readOffAddr# arr# (i2# +# unInt index3) s0 of
    (# s1, w #) -> case readOffAddr# arr# (i2# +# unInt index2) s1 of
      (# s2, x #) -> case readOffAddr# arr# (i2# +# unInt index1) s2 of
        (# s3, y #) -> case readOffAddr# arr# (i2# +# unInt index0) s3 of
          (# s4, z #) -> (# s4, Int256 w x y z #)
  where i2# = 4# *# i#

{-# INLINE writeOffAddr256# #-}
writeOffAddr256# :: Addr# -> Int# -> Int256 -> State# s -> State# s
writeOffAddr256# arr# i# (Int256 a b c d) =
  \s0 -> case writeOffAddr# arr# (i2# +# unInt index3) a s0 of
    s1 -> case writeOffAddr# arr# (i2# +# unInt index2) b s1 of
      s2 -> case writeOffAddr# arr# (i2# +# unInt index1) c s2 of
        s3 -> case writeOffAddr# arr# (i2# +# unInt index0) d s3 of
          s4 -> s4
  where i2# = 4# *# i#

{-# INLINE setOffAddr256# #-}
setOffAddr256# :: Addr# -> Int# -> Int# -> Int256 -> State# s -> State# s
setOffAddr256# = defaultSetOffAddr#

-- -----------------------------------------------------------------------------
-- Constants.

zeroInt256 :: Int256
zeroInt256 = Int256 0 0 0 0

oneInt256 :: Int256
oneInt256 = Int256 0 0 0 1

minusOneInt256 :: Int256
minusOneInt256 = Int256 maxBound maxBound maxBound maxBound

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
