{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.WideWord.Word128
  ( testWord128
  ) where

import Data.Bits ((.&.), (.|.), bit, complement, countLeadingZeros, countTrailingZeros, popCount, rotateL, rotateR, shiftL, shiftR, testBit, xor)
import Data.Int (Int16)
import Data.Word (Word32, Word64)
import Data.WideWord

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)


testWord128 :: Spec
testWord128 = describe "Word128:" $ do
  prop "constructor and accessors" $ \ (h, l) ->
    let w128 = Word128 h l in
    (word128Hi64 w128, word128Lo64 w128) `shouldBe` (h, l)

  prop "byte swap" $ \ (h, l) ->
    let w128 = byteSwapWord128 $ byteSwapWord128 (Word128 h l) in
    (word128Hi64 w128, word128Lo64 w128) `shouldBe` (h, l)

  prop "derivied Eq instance" $ \ (a1, a0, b1, b0) ->
    (Word128 a1 a0 == Word128 b1 b0) `shouldBe` (a1 == b1 && a0 == b0)

  prop "Ord instance" $ \ (a1, a0, b1, b0) ->
    compare (Word128 a1 a0) (Word128 b1 b0) `shouldBe` compare (mkInteger a1 a0) (mkInteger b1 b0)

  prop "show" $ \ (a1, a0) ->
    show (Word128 a1 a0) `shouldBe` show (mkInteger a1 a0)

  prop "read" $ \ (a1, a0) ->
    read (show $ Word128 a1 a0) `shouldBe` Word128 a1 a0

  prop "succ / pred" $ \ a0 -> do
    -- Only test on the lower word because `succ maxBound` and  `pred minBound`
    -- result in exceptions.
    toInteger128 (succ $ Word128 1 a0) `shouldBe` succ (mkInteger 1 a0)
    toInteger128 (pred $ Word128 1 a0) `shouldBe` pred (mkInteger 1 a0)

  prop "toEnum / fromEnum" $ \ (a0 :: Word32) -> do
    let w128 = Word128 0 (fromIntegral a0)
        e128 = fromEnum w128
    toInteger e128 `shouldBe` toInteger a0
    toInteger128 (toEnum e128 :: Word128) `shouldBe` toInteger a0

  prop "addition" $ \ (a1, a0, b1, b0) ->
    toInteger128 (Word128 a1 a0 + Word128 b1 b0) `shouldBe` correctWord128 (mkInteger a1 a0 + mkInteger b1 b0)

  prop "subtraction" $ \ (a1, a0, b1, b0) -> do
    let ai = mkInteger a1 a0
        bi = mkInteger b1 b0
        expected = ai + (1 `shiftL` 128) - bi
    toInteger128 (Word128 a1 a0 - Word128 b1 b0) `shouldBe` correctWord128 expected

  prop "multiplication" $ \ (a1, a0, b1, b0) ->
    toInteger128 (Word128 a1 a0 * Word128 b1 b0) `shouldBe` correctWord128 (mkInteger a1 a0 * mkInteger b1 b0)

  prop "negate" $ \ (a1, a0) ->
    toInteger128 (negate (Word128 a1 a0)) `shouldBe` correctWord128 (negate $ mkInteger a1 a0)

  prop "abs" $ \ (a1, a0) ->
    toInteger128 (abs (Word128 a1 a0)) `shouldBe` correctWord128 (abs $ mkInteger a1 a0)

  prop "signum" $ \ (a1, a0) ->
    toInteger128 (signum $ Word128 a1 a0) `shouldBe` signum (mkInteger a1 a0)

  prop "fromInteger" $ \ (a1, a0) -> do
    let w128 = fromInteger $ mkInteger a1 a0
    (word128Hi64 w128, word128Lo64 w128) `shouldBe` (a1, a0)

  prop "logical and/or/xor" $ \ (a1, a0, b1, b0) -> do
    toInteger128 (Word128 a1 a0 .&. Word128 b1 b0) `shouldBe` (mkInteger a1 a0 .&. mkInteger b1 b0)
    toInteger128 (Word128 a1 a0 .|. Word128 b1 b0) `shouldBe` (mkInteger a1 a0 .|. mkInteger b1 b0)
    toInteger128 (xor (Word128 a1 a0) (Word128 b1 b0)) `shouldBe` xor (mkInteger a1 a0) (mkInteger b1 b0)

  prop "complement" $ \ (a1, a0) ->
    toInteger128 (complement $ Word128 a1 a0) `shouldBe` mkInteger (complement a1) (complement a0)

  prop "logical shiftL" $ \ (a1, a0) shift ->
    let safeShift = if shift < 0 then 128 - (abs shift `mod` 128) else shift in
    toInteger128 (shiftL (Word128 a1 a0) shift) `shouldBe` correctWord128 (shiftL (mkInteger a1 a0) safeShift)

  prop "logical shiftR" $ \ (a1, a0) shift ->
    let expected = if shift < 0 then 0 else correctWord128 (shiftR (mkInteger a1 a0) shift) in
    toInteger128 (shiftR (Word128 a1 a0) shift) `shouldBe` expected

  -- Use `Int16` here to force a uniform distribution across the `Int16` range
  -- (standard QuickCkeck generator for `Int` doesn't give an even distribution).
  prop "logical rotateL" $ \ (a1, a0) (r :: Int16) -> do
    let rot = fromIntegral r
        i128 = mkInteger a1 a0
        expected
          | rot < 0 = 0
          | otherwise =
              correctWord128 (i128 `shiftL` erot + i128 `shiftR` (128 - (erot `mod` 128)))
              where
                erot
                  | rot < 0 = 128 - (abs rot `mod` 128)
                  | otherwise = rot `mod` 128
    toInteger128 (rotateL (Word128 a1 a0) rot) `shouldBe` expected

  prop "logical rotateR" $ \ (a1, a0) (r :: Int16) -> do
    let rot = fromIntegral r
        i128 = mkInteger a1 a0
        expected =
          correctWord128 $ i128 `shiftR` erot + i128 `shiftL` (128 - erot)
          where
            erot
              | rot < 0 = 128 - (abs rot `mod` 128)
              | otherwise = rot `mod` 128
    toInteger128 (rotateR (Word128 a1 a0) rot) `shouldBe` expected

  prop "testBit" $ \ (a1, a0) (b :: Int16) -> do
    let idx = fromIntegral b
        expected
          | idx < 0 = False
          | idx > 128 = False
          | otherwise = testBit (mkInteger a1 a0) idx
    testBit (Word128 a1 a0) idx `shouldBe` expected

  prop "bit" $ \ (b :: Int16) -> do
    let idx = fromIntegral b
        expected
          | idx < 0 = 0
          | idx >= 128 = 0
          | otherwise = bit idx
    toInteger128 (bit idx :: Word128) `shouldBe` expected

  prop "popCount" $ \ (a1, a0) ->
    popCount (Word128 a1 a0) `shouldBe` popCount (mkInteger a1 a0)

  prop "countLeadingZeros" $ \ (a1, a0) -> do
    let expected = if a1 == 0
                    then 64 + countLeadingZeros a0
                    else countLeadingZeros a1
    countLeadingZeros (Word128 a1 a0) `shouldBe` expected

  prop "countTrailingZeros" $ \ (a1, a0) -> do
    let expected = if a0 == 0
                    then 64 + countTrailingZeros a1
                    else countTrailingZeros a0
    countTrailingZeros (Word128 a1 a0) `shouldBe` expected

-- -----------------------------------------------------------------------------

mkInteger :: Word64 -> Word64 -> Integer
mkInteger a1 a0 = fromIntegral a1 `shiftL` 64 + fromIntegral a0

correctWord128 :: Integer -> Integer
correctWord128 i
  | i >= 0 && i <= maxWord128 = i
  | otherwise = i .&. maxWord128
  where
    maxWord128 = (1 `shiftL` 128) - 1

