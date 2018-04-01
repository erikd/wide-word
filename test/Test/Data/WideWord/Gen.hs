module Test.Data.WideWord.Gen where

import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.WideWord
import           Data.Word (Word8, Word16, Word32, Word64)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genInt8 :: Gen Int8
genInt8 =
  Gen.int8 Range.constantBounded

genInt16 :: Gen Int16
genInt16 =
  Gen.int16 Range.constantBounded

genInt32 :: Gen Int32
genInt32 =
  Gen.int32 Range.constantBounded

genInt64 :: Gen Int64
genInt64 =
  Gen.int64 Range.constantBounded

genInt128 :: Gen Int128
genInt128 =
  Int128 <$> genWord64 <*> genWord64

genWord8 :: Gen Word8
genWord8 =
  Gen.word8 Range.constantBounded

genWord16 :: Gen Word16
genWord16 =
  Gen.word16 Range.constantBounded

genWord32 :: Gen Word32
genWord32 =
  Gen.word32 Range.constantBounded

genWord64 :: Gen Word64
genWord64 =
  Gen.word64 Range.constantBounded

genWord128 :: Gen Word128
genWord128 =
  Word128 <$> genWord64 <*> genWord64
