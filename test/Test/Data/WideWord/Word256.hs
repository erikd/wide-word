{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Data.WideWord.Word256
  ( tests
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, when)

import Data.Binary qualified as Binary
import Data.Bits ((.&.), (.|.), bit, complement, countLeadingZeros, countTrailingZeros
                            , popCount, rotateL, rotateR, shiftL, shiftR, testBit, xor, finiteBitSize)
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Word (Word64, Word8)
import Data.WideWord

import Foreign (allocaBytes)
import Foreign.Storable (Storable (..))

import Hedgehog (Property, (===), discover)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Data.WideWord.Gen


-- Set the number of times to run each property test here.
propertyCount :: H.PropertyT IO () -> Property
propertyCount =
  H.withTests 10000 . H.property

prop_constructor_and_accessors :: Property
prop_constructor_and_accessors =
  propertyCount $ do
    (hi, m1, m0, lo) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    let w256 = Word256 hi m1 m0 lo
    (word256hi w256, word256m1 w256, word256m0 w256, word256lo w256) === (hi, m1, m0, lo)

{-
prop_byte_swap :: Property
prop_byte_swap =
  propertyCount $ do
    h <- H.forAll genWor256
    l <- H.forAll $ Gen.filter (/= h) genWord256
    let w256 = Word256 (word256Hi64 h) (word256Lo64 h) (word256Hi64 l) (word256Lo64 l)
        swapped = byteSwapWord256 w256
    (byteSwapWord256 swapped)
            === (w256)
-}


prop_derivied_eq_instance :: Property
prop_derivied_eq_instance =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    (b3, b2, b1, b0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    (Word256 a3 a2 a1 a0 == Word256 b3 b2 b1 b0) === (a3 == b3 && a2 == b2 && a1 == b1 && a0 == b0)

prop_ord_instance :: Property
prop_ord_instance =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    compare a b === compare (toInteger256 a) (toInteger256 b)

prop_show_instance :: Property
prop_show_instance =
  propertyCount $ do
    w256 <- H.forAll genWord256
    show w256 === show (toInteger256 w256)

prop_read_instance :: Property
prop_read_instance =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    read (show $ Word256 a3 a2 a1 a0) === Word256 a3 a2 a1 a0

prop_read_show :: Property
prop_read_show =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    H.tripping (Word256 a3 a2 a1 a0) show (Just . read)

prop_succ :: Property
prop_succ =
  propertyCount $ do
    w256 <- H.forAll $ Gen.filter (< maxBound) genWord256
    toInteger256 (succ w256) === succ (toInteger256 w256)

prop_pred :: Property
prop_pred =
  propertyCount $ do
    w256 <- H.forAll $ Gen.filter (> 0) genWord256
    toInteger256 (pred w256) === pred (toInteger256 w256)

prop_toEnum_fromEnum :: Property
prop_toEnum_fromEnum =
  propertyCount $ do
    a0 <- H.forAll genWord32
    let w256 = Word256 0 0 0 (fromIntegral a0)
        e256 = fromEnum w256
    -- On 32-bit architecture `a0` can exceed maxBound :: Int32
    -- making fromEnum illegal. So limiting this test to 64-bit.
    when (finiteBitSize (0 :: Word) == 64) $ do
      toInteger e256 === toInteger a0
      toInteger256 (toEnum e256 :: Word256) === toInteger a0

prop_addition :: Property
prop_addition =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    toInteger256 (a + b) === correctWord256 (toInteger256 a + toInteger256 b)

prop_subtraction :: Property
prop_subtraction =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    let ai = toInteger256 a
        bi = toInteger256 b
        expected = ai + (1 `shiftL` 256) - bi
    toInteger256 (a - b) === correctWord256 expected


prop_multiplication :: Property
prop_multiplication =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    toInteger256 (a * b) === correctWord256 (toInteger256 a * toInteger256 b)

prop_negate :: Property
prop_negate =
  propertyCount $ do
    w256 <- H.forAll genWord256
    toInteger256 (negate w256) === correctWord256 (negate $ toInteger256 w256)

prop_abs :: Property
prop_abs =
  propertyCount $ do
    w256 <- H.forAll genWord256
    toInteger256 (abs w256) === correctWord256 (abs $ toInteger256 w256)

prop_signum :: Property
prop_signum =
  propertyCount $ do
    w256 <- H.forAll genWord256
    toInteger256 (signum w256) === signum (toInteger256 w256)

prop_fromInteger :: Property
prop_fromInteger =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    let w256 = fromInteger $ mkInteger a3 a2 a1 a0
    (word256hi w256, word256m1 w256, word256m0 w256, word256lo w256) === (a3, a2, a1, a0)

prop_bitwise_and :: Property
prop_bitwise_and =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    toInteger256 (a .&. b) === (toInteger256 a .&. toInteger256 b)

prop_bitwise_or :: Property
prop_bitwise_or =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    toInteger256 (a .|. b) === (toInteger256 a .|. toInteger256 b)

prop_bitwise_xor :: Property
prop_bitwise_xor =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord256 <*> genWord256
    toInteger256 (xor a b) === xor (toInteger256 a) (toInteger256 b)

prop_complement :: Property
prop_complement =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    toInteger256 (complement $ Word256 a3 a2 a1 a0) === mkInteger (complement a3) (complement a2) (complement a1) (complement a0)

prop_logical_shift_left :: Property
prop_logical_shift_left =
  propertyCount $ do
    w256 <- H.forAll genWord256
    shift <- H.forAll $ Gen.int (Range.linear 0 260)
    toInteger256 (shiftL w256 shift) === correctWord256 (shiftL (toInteger256 w256) shift)

prop_logical_shift_right :: Property
prop_logical_shift_right =
  propertyCount $ do
    w256 <- H.forAll genWord256
    shift <- H.forAll $ Gen.int (Range.linear 0 260)
    toInteger256 (shiftR w256 shift) === shiftR (toInteger256 w256) shift

prop_logical_rotate_left :: Property
prop_logical_rotate_left =
  propertyCount $ do
    w256 <- H.forAll genWord256
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    let i256 = toInteger256 w256
        expected =
          correctWord256 (i256 `shiftL` erot + i256 `shiftR` (256 - erot))
          where
            erot
              | rot < 0 = 256 - (abs rot `mod` 256)
              | otherwise = rot `mod` 256
    toInteger256 (rotateL w256 rot) === expected

prop_logical_rotate_right :: Property
prop_logical_rotate_right =
  propertyCount $ do
    w256 <- H.forAll genWord256
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    let i256 = toInteger256 w256
        expected =
          correctWord256 $ i256 `shiftR` erot + i256 `shiftL` (256 - erot)
          where
            erot
              | rot < 0 = 256 - (abs rot `mod` 256)
              | otherwise = rot `mod` 256
    toInteger256 (rotateR w256 rot) === expected

prop_shift_opposite :: Property
prop_shift_opposite =
  propertyCount $ do
    w256 <- H.forAll genWord256
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    shiftL w256 rot === shiftR w256 (negate rot)

prop_testBit :: Property
prop_testBit =
  propertyCount $ do
    w256 <- H.forAll genWord256
    idx <- H.forAll $ Gen.int (Range.linearFrom 0 (-200) 200)
    let expected
          | idx < 0 = False
          | idx >= 256 = False
          | otherwise = testBit (toInteger256 w256) idx
    testBit w256 idx === expected


prop_bit :: Property
prop_bit =
  propertyCount $ do
    b <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    let idx = fromIntegral b
        expected
          | idx < 0 = 0
          | idx >= 256 = 0
          | otherwise = bit idx
    toInteger256 (bit idx :: Word256) === expected


prop_popCount :: Property
prop_popCount =
  propertyCount $ do
    w256 <- H.forAll genWord256
    popCount w256 === popCount (toInteger256 w256)

prop_countLeadingZeros :: Property
prop_countLeadingZeros =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    let expected =
          case (a3, a2, a1, a0) of
            (0, 0, 0, _) -> 192 + countLeadingZeros a0
            (0, 0, _, _) -> 128 + countLeadingZeros a1
            (0, _, _, _) -> 64 + countLeadingZeros a2
            (_, _, _, _) -> countLeadingZeros a3
    countLeadingZeros (Word256 a3 a2 a1 a0) === expected

prop_countTrailingZeros :: Property
prop_countTrailingZeros =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genWord64 <*> genWord64 <*> genWord64 <*> genWord64
    let expected =
          case (a3, a2, a1, a0) of
            (_, 0, 0, 0) -> 192 + countTrailingZeros a3
            (_, _, 0, 0) -> 128 + countTrailingZeros a2
            (_, _, _, 0) -> 64 + countTrailingZeros a1
            (_, _, _, _) -> countTrailingZeros a0
    countTrailingZeros (Word256 a3 a2 a1 a0) === expected

-- Don't need to test `quot` or `rem` because they are implemented by applying
-- `fst` or `snd` to the output of `quotRem`.
prop_quotRem :: Property
prop_quotRem =
  propertyCount $ do
    num <- H.forAll genWord256
    den <- H.forAll $ Gen.filter (/= 0) genWord256
    let (q, r) = quotRem num den
    (toInteger256 q, toInteger256 r) === quotRem (toInteger256 num) (toInteger256 den)

prop_divMod :: Property
prop_divMod =
  propertyCount $ do
    num <- H.forAll genWord256
    den <- H.forAll $ Gen.filter (/= 0) genWord256
    let (d, m) = divMod num den
    (toInteger256 d, toInteger256 m) === divMod (toInteger256 num) (toInteger256 den)

prop_roundtrip_binary :: Property
prop_roundtrip_binary =
  propertyCount $ do
    w256 <- H.forAll genWord256
    H.tripping w256 Binary.encode (Just . Binary.decode)

prop_peek_and_poke :: Property
prop_peek_and_poke =
  propertyCount $ do
    w256 <- H.forAll genWord256
    ar <- liftIO $
            allocaBytes (sizeOf zeroWord256) $ \ ptr -> do
              poke ptr w256
              peek ptr
    toInteger256 ar === toInteger256 w256

prop_peekElemOff_pokeElemOff :: Property
prop_peekElemOff_pokeElemOff =
  propertyCount $ do
    a256 <- H.forAll genWord256
    b256 <- H.forAll genWord256
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroWord256) $ \ ptr -> do
                    pokeElemOff ptr 0 a256
                    pokeElemOff ptr 1 b256
                    (,) <$> peekElemOff ptr 0 <*>  peekElemOff ptr 1
    (toInteger256 ar, toInteger256 br) === (toInteger256 a256, toInteger256 b256)


prop_ToFromPrimArray :: Property
prop_ToFromPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $
      Gen.list (fromIntegral <$> (Range.linearBounded :: Range.Range Word8)) genWord256
    as === primArrayToList (primArrayFromList as)

prop_WriteReadPrimArray :: Property
prop_WriteReadPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $ Gen.list (Range.linear 1 256) genWord256
    unless (null as) $ do
      let len = length as
          arr = primArrayFromList as
      i <- (`mod` len) <$> H.forAll (Gen.int (Range.linear 0 (len - 1)))
      new <- H.forAll genWord256
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
    a256 <- H.forAll genWord256
    b256 <- H.forAll genWord256
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroWord256) $ \ ptr -> do
                    writeOffPtr ptr 0 a256
                    writeOffPtr ptr 1 b256
                    (,) <$> readOffPtr ptr 0 <*> readOffPtr ptr 1
    (ar, br) === (a256, b256)

-- -----------------------------------------------------------------------------

mkInteger :: Word64 -> Word64 -> Word64 -> Word64 -> Integer
mkInteger a3 a2 a1 a0 =
  fromIntegral a3 `shiftL` 192 +  fromIntegral a2 `shiftL` 128
    + fromIntegral a1 `shiftL` 64 + fromIntegral a0

correctWord256 :: Integer -> Integer
correctWord256 i
  | i >= 0 && i <= maxWord256 = i
  | otherwise = i .&. maxWord256
  where
    maxWord256 = (1 `shiftL` 256) - 1

toInteger256 :: Word256 -> Integer
toInteger256 = toInteger

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
