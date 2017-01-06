{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.WideWord.Word128
  ( testWord128
  ) where

import Data.Bits (shiftL)
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
    toInteger128 (toEnum e128 :: Word128) `shouldBe` fromIntegral a0

-- -----------------------------------------------------------------------------

mkInteger :: Word64 -> Word64 -> Integer
mkInteger a1 a0 = fromIntegral a1 `shiftL` 64 + fromIntegral a0
