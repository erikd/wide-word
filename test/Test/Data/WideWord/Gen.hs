module Test.Data.WideWord.Gen where

import           Data.Bits (shiftL)
import           Data.WideWord
import           Data.Word (Word32, Word64)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genInt128 :: Gen Int128
genInt128 =
  Int128 <$> genBiasedWord64 <*> genBiasedWord64

genWord32 :: Gen Word32
genWord32 =
  Gen.word32 Range.constantBounded

genWord64 :: Gen Word64
genWord64 =
  Gen.word64 Range.constantBounded

-- | Generate 'Word64' in one of five categories;
--    * the full range
--    * small values near zero
--    * large values near maxBound :: Word64
--    * values near maxBound / 2 :: Word64
--    * values near maxBound :: Word32
genBiasedWord64 :: Gen Word64
genBiasedWord64 =
  Gen.choice
    [ Gen.word64 (Range.linear 0 maxBound)
    , Gen.word64 (Range.linear 0 100)
    , (-) maxBound <$> Gen.word64 (Range.linear 0 100)
    , Gen.word64 (Range.linear (halfMax - 100) (halfMax + 100))
    , Gen.word64 (Range.linear (bits32 - 100) (bits32 + 100))
    ]
  where
    bits32 :: Word64
    bits32 = 1 `shiftL` 32

    halfMax :: Word64
    halfMax = maxBound `div` 2

genWord128 :: Gen Word128
genWord128 =
  Word128 <$> genBiasedWord64 <*> genBiasedWord64
