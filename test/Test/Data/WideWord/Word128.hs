{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.WideWord.Word128
  ( testWord128
  ) where

import Data.Bits (shiftL)
import Data.Word (Word64)
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
    compare (Word128 a1 a0) (Word128 b1 b0) `shouldBe` compare (toInteger128 a1 a0) (toInteger128 b1 b0)

  prop "show" $ \ (a1, a0) ->
    show (Word128 a1 a0) `shouldBe` show (toInteger128 a1 a0)

  prop "read" $ \ (a1, a0) -> do
    let i = show (toInteger128 a1 a0)
    read i `shouldBe` Word128 a1 a0

-- -----------------------------------------------------------------------------

toInteger128 :: Word64 -> Word64 -> Integer
toInteger128 a1 a0 = fromIntegral a1 `shiftL` 64 + fromIntegral a0
