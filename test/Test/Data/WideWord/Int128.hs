{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.WideWord.Int128
  ( testInt128
  ) where

import Control.Exception (evaluate)

import Data.Bits ((.&.), (.|.), bit, complement, countLeadingZeros, countTrailingZeros, popCount, rotateL, rotateR, shiftL, shiftR, testBit, xor)
import Data.Int (Int16)
import Data.Word (Word32, Word64)
import Data.WideWord

import Foreign (allocaBytes)
import Foreign.Storable (Storable (..))

import Test.Hspec (Spec, describe, errorCall, it, shouldBe, shouldThrow)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Modifiers (NonZero (..))


testInt128 :: Spec
testInt128 = describe "Int128:" $ do
  prop "constructor and accessors" $ \ (h, l) ->
    let i128 = Int128 h l in
    (int128Hi64 i128, int128Lo64 i128) `shouldBe` (h, l)

  prop "byte swap" $ \ (h, l) ->
    let i128 = byteSwapInt128 $ byteSwapInt128 (Int128 h l) in
    (int128Hi64 i128, int128Lo64 i128) `shouldBe` (h, l)

  prop "derivied Eq instance" $ \ (a1, a0, b1, b0) ->
    (Int128 a1 a0 == Int128 b1 b0) `shouldBe` (a1 == b1 && a0 == b0)

  prop "toInteger" $ \ (a1, a0) ->
    toInteger (Int128 a1 a0) `shouldBe` mkInteger a1 a0

  prop "negate" $ \ (a1, a0) ->
    toInteger (negate (Int128 a1 a0)) `shouldBe` negate (mkInteger a1 a0)

  prop "fromInteger" $ \ (a1, a0) -> do
    let i128 = fromInteger $ mkInteger a1 a0
    (int128Hi64 i128, int128Lo64 i128) `shouldBe` (a1, a0)

  prop "Ord instance" $ \ (a1, a0, b1, b0) ->
    compare (Int128 a1 a0) (Int128 b1 b0) `shouldBe` compare (mkInteger a1 a0) (mkInteger b1 b0)

  prop "show / read" $ \ (a1, a0) ->
    toInteger (read (show $ Int128 a1 a0) :: Int128) `shouldBe` mkInteger a1 a0

  prop "succ" $ \ (a1, a0) -> do
    let i128 = Int128 a1 a0
    if i128 == maxBound
      then evaluate (succ i128) `shouldThrow` errorCall "Enum.succ{Int128}: tried to take `succ' of maxBound"
      else toInteger128 (succ i128) `shouldBe` succ (mkInteger a1 a0)

  prop "pred" $ \ (a1, a0) -> do
    let i128 = Int128 a1 a0
    if i128 == minBound
      then evaluate (pred i128) `shouldThrow` errorCall "Enum.pred{Int128}: tried to take `pred' of minBound"
      else toInteger128 (pred i128) `shouldBe` pred (mkInteger a1 a0)

  it "succ maxBound throws error" $
    evaluate (succ (maxBound :: Int128)) `shouldThrow` errorCall "Enum.succ{Int128}: tried to take `succ' of maxBound"

  it "pred minBount throws error" $
    evaluate (pred (minBound :: Int128)) `shouldThrow` errorCall "Enum.pred{Int128}: tried to take `pred' of minBound"

  prop "toEnum / fromEnum" $ \ (a0 :: Word32) -> do
    let i128 = Int128 0 (fromIntegral a0)
        e128 = fromEnum i128
    toInteger e128 `shouldBe` toInteger a0
    toInteger (toEnum e128 :: Int128) `shouldBe` toInteger a0

  prop "complement" $ \ (a1, a0) ->
    toInteger (complement $ Int128 a1 a0) `shouldBe` mkInteger (complement a1) (complement a0)

  prop "negate" $ \ (a1, a0) ->
    toInteger (negate (Int128 a1 a0)) `shouldBe` negate (mkInteger a1 a0)

  prop "abs" $ \ (a1, a0) ->
    toInteger (abs (Int128 a1 a0)) `shouldBe` abs (mkInteger a1 a0)

  prop "signum" $ \ (a1, a0) ->
    toInteger (signum $ Int128 a1 a0) `shouldBe` signum (mkInteger a1 a0)

  prop "logical and/or/xor" $ \ (a1, a0, b1, b0) -> do
    toInteger (Int128 a1 a0 .&. Int128 b1 b0) `shouldBe` (mkInteger a1 a0 .&. mkInteger b1 b0)
    toInteger (Int128 a1 a0 .|. Int128 b1 b0) `shouldBe` (mkInteger a1 a0 .|. mkInteger b1 b0)
    toInteger (xor (Int128 a1 a0) (Int128 b1 b0)) `shouldBe` xor (mkInteger a1 a0) (mkInteger b1 b0)

  prop "testBit" $ \ (a1, a0) (b :: Int16) -> do
    let idx = fromIntegral b
        expected
          | idx < 0 = False
          | idx > 128 = False
          | otherwise = testBit (mkInteger a1 a0) idx
    testBit (Int128 a1 a0) idx `shouldBe` expected

  prop "bit" $ \ (b :: Int16) -> do
    let idx = fromIntegral b
        expected
          | idx < 0 = 0
          | idx >= 128 = 0
          | idx == 127 = toInteger128 (minBound :: Int128)
          | otherwise = bit idx
    toInteger (bit idx :: Int128) `shouldBe` expected

  prop "popCount" $ \ (a1, a0) ->
    popCount (Int128 a1 a0) `shouldBe` popCount a1 + popCount a0

  prop "countLeadingZeros" $ \ (a1, a0) -> do
    let expected = if a1 == 0
                    then 64 + countLeadingZeros a0
                    else countLeadingZeros a1
    countLeadingZeros (Int128 a1 a0) `shouldBe` expected

  prop "countTrailingZeros" $ \ (a1, a0) -> do
    let expected = if a0 == 0
                    then 64 + countTrailingZeros a1
                    else countTrailingZeros a0
    countTrailingZeros (Int128 a1 a0) `shouldBe` expected


  prop "addition" $ \ (a1, a0, b1, b0) ->
    toInteger (Int128 a1 a0 + Int128 b1 b0) `shouldBe` correctInt128 (mkInteger a1 a0 + mkInteger b1 b0)

  prop "subtraction" $ \ (a1, a0, b1, b0) ->
    toInteger (Int128 a1 a0 - Int128 b1 b0) `shouldBe` correctInt128 (mkInteger a1 a0 - mkInteger b1 b0)

  prop "multiplication" $ \ (a1, a0, b1, b0) ->
    toInteger (Int128 a1 a0 * Int128 b1 b0) `shouldBe` correctInt128 (mkInteger a1 a0 * mkInteger b1 b0)

  prop "logical shiftL" $ \ (a1, a0) shift ->
    let safeShift = if shift < 0 then 128 - (abs shift `mod` 128) else shift in
    toInteger (shiftL (Int128 a1 a0) shift) `shouldBe` correctInt128 (shiftL (mkInteger a1 a0) safeShift)

  prop "logical shiftR" $ \ (a1, a0) shift ->
    let expected = if shift < 0 then 0 else correctInt128 (shiftR (mkInteger a1 a0) shift) in
    toInteger (shiftR (Int128 a1 a0) shift) `shouldBe` expected

  -- Use `Int16` here to force a uniform distribution across the `Int16` range
  -- (standard QuickCkeck generator for `Int` doesn't give an even distribution).
  prop "logical rotateL" $ \ (a1, a0) (r :: Int16) -> do
    let rot = fromIntegral r
    toInteger (rotateL (Int128 a1 a0) rot) `shouldBe` correctInt128 (toInteger $ rotateL (Word128 a1 a0) rot)

  prop "logical rotateR" $ \ (a1, a0) (r :: Int16) -> do
    let rot = fromIntegral r
    toInteger (rotateR (Int128 a1 a0) rot) `shouldBe` correctInt128 (toInteger $ rotateR (Word128 a1 a0) rot)

  prop "quotRem" $ \ (a1, a0, NonZero b1, b0) -> do
    let (aq128, ar128) = quotRem (Int128 a1 a0) (Int128 b1 b0)
    (toInteger aq128, toInteger ar128) `shouldBe` quotRem (mkInteger a1 a0) (mkInteger b1 b0)

  prop "divMod" $ \ (a1, a0, NonZero b1, b0) -> do
    let (aq128, ar128) = divMod (Int128 a1 a0) (Int128 b1 b0)
    (toInteger aq128, toInteger ar128) `shouldBe` divMod (mkInteger a1 a0) (mkInteger b1 b0)

  prop "peek / poke" $ \ (a1, a0) -> do
    ar <- allocaBytes (sizeOf zeroInt128) $ \ ptr -> do
                    poke ptr $ Int128 a1 a0
                    peek ptr
    toInteger128 ar `shouldBe` mkInteger a1 a0

  prop "peekElemOff / pokeElemOff" $ \ (a1, a0, b1, b0) -> do
    (ar, br) <- allocaBytes (2 * sizeOf zeroInt128) $ \ ptr -> do
                    pokeElemOff ptr 0 $ Int128 a1 a0
                    pokeElemOff ptr 1 $ Int128 b1 b0
                    (,) <$> peekElemOff ptr 0 <*>  peekElemOff ptr 1
    (toInteger128 ar, toInteger128 br) `shouldBe` (mkInteger a1 a0, mkInteger b1 b0)



-- -----------------------------------------------------------------------------

-- Convert an `Integer` to the `Integer` with the same bit pattern as the
-- corresponding `Int128`.
correctInt128 :: Integer -> Integer
correctInt128 x
  | x >= minBoundInt128 && x <= maxBoundInt128 = x
  | otherwise = toInteger (fromIntegral x :: Int128)
  where
    minBoundInt128 = fromIntegral (minBound :: Int128)
    maxBoundInt128 = fromIntegral (maxBound :: Int128)

mkInteger :: Word64 -> Word64 -> Integer
mkInteger a1 a0
  | testBit a1 63 = negate (fromIntegral (complement a1) `shiftL` 64 + fromIntegral (complement a0) + 1)
  | otherwise = fromIntegral a1 `shiftL` 64 + fromIntegral a0


toInteger128 :: Int128 -> Integer
toInteger128 = toInteger
