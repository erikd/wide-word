{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Data.WideWord.Word64
  ( tests
  ) where

import           Control.Exception (ArithException, SomeException, evaluate, try)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless)

import           Data.Bifunctor (first)
import qualified Data.Binary as Binary
import           Data.Bits ((.&.), (.|.), bit, complement, countLeadingZeros, countTrailingZeros
                            , popCount, rotateL, rotateR, shiftL, shiftR, testBit, xor)
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr
import           Data.Word (Word8, Word64, byteSwap64)
import           Data.WideWord

import           Foreign (allocaBytes)
import           Foreign.Storable (Storable (..))

import           Hedgehog (Property, (===), discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Data.WideWord.Gen

-- The other WideWord types are implemented in terms of Word64, so we test Word64 here
-- to make sure that we get the same results across platorms and with 32 bit architectures.

-- Set the number of times to run each property test here.
propertyCount :: H.PropertyT IO () -> Property
propertyCount =
  H.withTests 10000 . H.property

prop_byte_swap :: Property
prop_byte_swap =
  propertyCount $ do
    w <- H.forAll genWord64
    byteSwap64 (byteSwap64 w) === w

prop_derivied_eq_instance :: Property
prop_derivied_eq_instance =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    (a == b) === (word64Hi32 a == word64Hi32 b && word64Lo32 a == word64Lo32 b)

prop_ord_instance :: Property
prop_ord_instance =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    compare a b === compare (toInteger64 a) (toInteger64 b)

prop_show_instance :: Property
prop_show_instance =
  propertyCount $ do
    w64 <- H.forAll genWord64
    show w64 === show (toInteger64 w64)

prop_read_instance :: Property
prop_read_instance =
  propertyCount $ do
    w64 <- H.forAll genWord64
    read (show w64) === w64

prop_read_show :: Property
prop_read_show =
  propertyCount $ do
    w64 <- H.forAll genWord64
    H.tripping w64 show (Just . read)

prop_succ :: Property
prop_succ =
  propertyCount $ do
    w64 <- H.forAll genWord64
    res <- liftIO (fmap toInteger64 <$> tryEvaluate (succ w64))
    res === if w64 == maxBound
              then Left "Enum.succ{Word64}: tried to take `succ' of maxBound"
              else Right (succ $ toInteger64 w64)

prop_pred :: Property
prop_pred =
  propertyCount $ do
    w64 <- H.forAll $ Gen.filter (> 0) genWord64
    res <- liftIO (fmap toInteger64 <$> tryEvaluate (pred w64))
    res === if w64 == 0
              then Left "Enum.pred{Word64}: tried to take `pred' of minBound"
              else Right $ pred (toInteger64 w64)

prop_toEnum_fromEnum :: Property
prop_toEnum_fromEnum =
  propertyCount $ do
    -- Need to rande limit the Word64, because `fromEnum` is limited to the positive part
    -- of the range of Int and we need to support 32 bit systems.
    w64 <- mkWord64 0 <$> H.forAll (Gen.filter (<= 0x7fffffff) genWord32)
    let e64 = fromEnum w64
    toInteger e64 === toInteger w64
    toInteger64 (toEnum e64 :: Word64) === toInteger w64

prop_addition :: Property
prop_addition =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    toInteger64 (a + b) === correctWord64 (toInteger64 a + toInteger64 b)

prop_subtraction :: Property
prop_subtraction =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    let ai = toInteger64 a
        bi = toInteger64 b
        expected = ai + (1 `shiftL` 64) - bi
    toInteger64 (a - b) === correctWord64 expected

prop_multiplication :: Property
prop_multiplication =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    toInteger64 (a * b) === correctWord64 (toInteger64 a * toInteger64 b)

prop_negate :: Property
prop_negate =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    toInteger64 (negate w64) === correctWord64 (negate $ toInteger64 w64)

prop_abs :: Property
prop_abs =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    toInteger64 (abs w64) === correctWord64 (abs $ toInteger64 w64)

prop_signum :: Property
prop_signum =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    toInteger64 (signum w64) === signum (toInteger64 w64)

prop_fromInteger :: Property
prop_fromInteger =
  propertyCount $ do
    i64 <- H.forAll $ Gen.integral (Range.linear 0 (fromIntegral (maxBound :: Word64) :: Integer))
    H.tripping i64 fromInteger (Just . toInteger64)

prop_bitwise_and :: Property
prop_bitwise_and =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    toInteger64 (a .&. b) === (toInteger64 a .&. toInteger64 b)

prop_bitwise_or :: Property
prop_bitwise_or =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    toInteger64 (a .|. b) === (toInteger64 a .|. toInteger64 b)

prop_bitwise_xor :: Property
prop_bitwise_xor =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    toInteger64 (xor a b) === xor (toInteger64 a) (toInteger64 b)

prop_complement :: Property
prop_complement =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    complement (complement w64) === w64

prop_logical_shift_left :: Property
prop_logical_shift_left =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    shift <- H.forAll $ Gen.int (Range.linear 0 130)
    toInteger64 (shiftL w64 shift) === correctWord64 (shiftL (toInteger64 w64) shift)

prop_logical_shift_right :: Property
prop_logical_shift_right =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    shift <- H.forAll $ Gen.int (Range.linear 0 130)
    toInteger64 (shiftR w64 shift) === shiftR (toInteger64 w64) shift

prop_logical_rotate_left :: Property
prop_logical_rotate_left =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    -- Actually testing the default compiler implementation so range must be valid.
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-63) 500)
    let i64 = toInteger64 w64
        expected =
              correctWord64 (i64 `shiftL` erot + i64 `shiftR` (64 - (erot `mod` 64)))
              where
                erot
                  | rot == 0 = 0
                  | rot < 0 = 64 - abs rot `mod` 64
                  | otherwise = rot `mod` 64
    toInteger64 (rotateL w64 rot) === expected

prop_logical_rotate_right :: Property
prop_logical_rotate_right =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-20000) 20000)
    let i64 = toInteger64 w64
        expected =
          correctWord64 (i64 `shiftR` erot + i64 `shiftL` (64 - erot))
          where
            erot
              | rot < 0 = 64 - abs rot `mod` 64
              | otherwise = rot `mod` 64
    toInteger64 (rotateR w64 rot) === expected

prop_testBit :: Property
prop_testBit =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    -- Actually testing the default compiler/machine implementation so range must be valid.
    idx <- H.forAll $ Gen.int (Range.linear 0 63)
    testBit w64 idx === testBit (toInteger64 w64) idx

prop_bit :: Property
prop_bit =
  propertyCount $ do
    -- Actually testing the default compiler/machine implementation so range must be valid.
    idx <- H.forAll $ Gen.int (Range.linear 0 63)
    toInteger64 (bit idx :: Word64) === (bit idx :: Integer)
    toInteger64 ((bit idx :: Word64) - 1) === ((bit idx - 1) :: Integer)

prop_popCount :: Property
prop_popCount =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    popCount w64 === popCount (toInteger64 w64)

prop_countLeadingZeros :: Property
prop_countLeadingZeros =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    let a0 = word64Lo32 w64
        a1 = word64Hi32 w64
    let expected = if a1 == 0
                    then 32 + countLeadingZeros a0
                    else countLeadingZeros a1
    countLeadingZeros w64 === expected

prop_countTrailingZeros :: Property
prop_countTrailingZeros =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    let a0 = word64Lo32 w64
        a1 = word64Hi32 w64
    let expected = if a0 == 0
                    then 32 + countTrailingZeros a1
                    else countTrailingZeros a0
    countTrailingZeros w64 === expected

-- Don't need to test `quot` or `rem` because they are implemented by applying
-- `fst` or `snd` to the output of `quotRem`.
prop_quotRem :: Property
prop_quotRem =
  propertyCount $ do
    num <- H.forAll genBiasedWord64
    den <- H.forAll $ Gen.filter (/= 0) genBiasedWord64
    let (q, r) = quotRem num den
    (toInteger64 q, toInteger64 r) === quotRem (toInteger64 num) (toInteger64 den)

prop_divMod :: Property
prop_divMod =
  propertyCount $ do
    num <- H.forAll genBiasedWord64
    den <- H.forAll $ Gen.filter (/= 0) genWord64
    let (d, m) = divMod num den
    (toInteger64 d, toInteger64 m) === divMod (toInteger64 num) (toInteger64 den)

prop_roundtrip_binary :: Property
prop_roundtrip_binary =
  propertyCount $ do
    w64 <- H.forAll genWord128
    H.tripping w64 Binary.encode (Just . Binary.decode)

prop_peek_and_poke :: Property
prop_peek_and_poke =
  propertyCount $ do
    w64 <- H.forAll genBiasedWord64
    ar <- liftIO $
            allocaBytes (sizeOf zeroWord64) $ \ ptr -> do
              poke ptr w64
              peek ptr
    toInteger64 ar === toInteger64 w64

prop_peekElemOff_pokeElemOff :: Property
prop_peekElemOff_pokeElemOff =
  propertyCount $ do
    a64 <- H.forAll genWord64
    b64 <- H.forAll genWord64
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroWord64) $ \ ptr -> do
                    pokeElemOff ptr 0 a64
                    pokeElemOff ptr 1 b64
                    (,) <$> peekElemOff ptr 0 <*>  peekElemOff ptr 1
    (toInteger64 ar, toInteger64 br) === (toInteger64 a64, toInteger64 b64)

prop_ToFromPrimArray :: Property
prop_ToFromPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $
      Gen.list (fromIntegral <$> (Range.linearBounded :: Range.Range Word8)) genWord64
    as === primArrayToList (primArrayFromList as)

prop_WriteReadPrimArray :: Property
prop_WriteReadPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $ Gen.list (Range.linear 1 256) genWord64
    unless (null as) $ do
      let len = length as
          arr = primArrayFromList as
      i <- (`mod` len) <$> H.forAll (Gen.int (Range.linear 0 (len - 1)))
      new <- H.forAll genWord64
      props <- liftIO $ do
        marr <- unsafeThawPrimArray arr
        prev <- readPrimArray marr i
        let prevProp = prev === (as !! i)
        writePrimArray marr i new
        cur <- readPrimArray marr i
        setPrimArray marr i 1 prev
        arr' <- unsafeFreezePrimArray marr
        return [prevProp, cur === new, arr === arr']
      sequence_ props

prop_readOffPtr_writeOffPtr :: Property
prop_readOffPtr_writeOffPtr =
  propertyCount $ do
    a64 <- H.forAll genWord64
    b64 <- H.forAll genWord64
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroWord64) $ \ ptr -> do
                    writeOffPtr ptr 0 a64
                    writeOffPtr ptr 1 b64
                    (,) <$> readOffPtr ptr 0 <*> readOffPtr ptr 1
    (ar, br) === (a64, b64)

prop_plusCarrySum :: Property
prop_plusCarrySum =
  propertyCount $ do
    a <- H.forAll genBiasedWord64
    b <- H.forAll genBiasedWord64
    let (carry, s) = plusCarrySum a b
    toInteger64 carry `shiftL` 64 + toInteger64 s === toInteger64 a + toInteger64 b

prop_quotRem2Word64 :: Property
prop_quotRem2Word64 =
  propertyCount $ do
    (num1, num0) <- H.forAll $ (,) <$> genWord64 <*>  genWord64
    -- Denominator must be greater than the most significant part of the numerator.
    -- If its not, the quotient is not big enough to hold the result.
    den <- H.forAll $ Gen.filter (\ w -> w /= 0 && w > num1) genWord64
    H.assert (den > num1)
    let (q, r) = quotRem2Word64 num1 num0 den
    (toInteger64 q, toInteger64 r) === quotRem (toInteger $ Word128 num1 num0) (toInteger64 den)

prop_timesCarryProd :: Property
prop_timesCarryProd =
  propertyCount $ do
    a <- H.forAll genBiasedWord64
    b <- H.forAll genBiasedWord64
    let (carry, p) = timesCarryProd a b
    toInteger64 carry `shiftL` 64 + toInteger64 p === toInteger64 a * toInteger64 b

prop_subCarryDiff :: Property
prop_subCarryDiff =
  propertyCount $ do
    a <- H.forAll genBiasedWord64
    b <- H.forAll genBiasedWord64
    let (carry, d) = subCarryDiff a b
    if a >= b
      then (carry, toInteger64 d) === (0, toInteger64 a - toInteger64 b)
      else (carry, toInteger64 d) === (1, 1 + fromIntegral (maxBound :: Word64) - toInteger64 b + toInteger64 a)

prop_subDiffCarry_ok :: Property
prop_subDiffCarry_ok =
  propertyCount $ do
    a <- H.forAll genBiasedWord64
    b <- H.forAll genBiasedWord64
    let (actualC, actualD) = subCarryDiff a b
    let (expectedC, expectedD) =
          if (a >= b)
            then (zeroWord64, a - b)
            else (oneWord64, a + maxBound + 1 - b)
    (actualC, actualD) === (expectedC, expectedD)

-- -----------------------------------------------------------------------------

correctWord64 :: Integer -> Integer
correctWord64 i
  | i >= 0 && i <= maxWord64 = i
  | otherwise = i .&. maxWord64
  where
    maxWord64 = (1 `shiftL` 64) - 1

showArithException :: ArithException -> String
showArithException = show

toInteger64 :: Word64 -> Integer
toInteger64 = toInteger

tryEvaluate :: a -> IO (Either String a)
tryEvaluate x = do
  first renderException <$> try (evaluate x)
  where
    renderException :: SomeException -> String
    renderException = show

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkSequential $$discover
