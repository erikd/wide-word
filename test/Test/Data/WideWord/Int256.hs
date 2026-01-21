{-# LANGUAGE TemplateHaskell #-}
module Test.Data.WideWord.Int256
  ( tests
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless)

import qualified Data.Binary as Binary
import           Data.Bits ((.&.), (.|.), bit, complement, countLeadingZeros, countTrailingZeros
                            , popCount, rotateL, rotateR, shiftL, shiftR, testBit, xor)
import           Data.Int (Int32)
import           Data.Primitive.PrimArray
import           Data.Primitive.Ptr
import           Data.Word (Word64, Word8)
import           Data.WideWord

import           Foreign (allocaBytes)
import           Foreign.Storable (Storable (..))

import           Hedgehog (Property, (===), discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Data.WideWord.Gen


-- Set the number of times to run each property test here.
propertyCount :: H.PropertyT IO () -> Property
propertyCount =
  H.withTests 10000 . H.property

prop_constructor_and_accessors :: Property
prop_constructor_and_accessors =
  propertyCount $ do
    (hi, m1, m0, lo) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    let w256 = Int256 hi m1 m0 lo
    (int256hi w256, int256m1 w256, int256m0 w256, int256lo w256) === (hi, m1, m0, lo)

prop_byte_swap :: Property
prop_byte_swap =
  propertyCount $ do
    h <- H.forAll genInt256
    l <- H.forAll $ Gen.filter (/= h) genInt256
    let i256 = Int256 (int256hi h) (int256m0 h) (int256m1 l) (int256lo l)
        swapped = byteSwapInt256 i256
    (byteSwapInt256 swapped)
            === (i256)

prop_derivied_eq_instance :: Property
prop_derivied_eq_instance =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    (b3, b2, b1, b0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    (Int256 a3 a2 a1 a0 == Int256 b3 b2 b1 b0) === (a3 == b3 && a2 == b2 && a1 == b1 && a0 == b0)

prop_ord_instance :: Property
prop_ord_instance =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    compare a b === compare (toInteger256 a) (toInteger256 b)

prop_show_instance :: Property
prop_show_instance =
  propertyCount $ do
    i256 <- H.forAll genInt256
    show i256 === show (toInteger256 i256)

prop_read_instance :: Property
prop_read_instance =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    read (show $ Int256 a3 a2 a1 a0) === Int256 a3 a2 a1 a0

prop_read_show :: Property
prop_read_show =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    H.tripping (Int256 a3 a2 a1 a0) show (Just . read)

prop_succ :: Property
prop_succ =
  propertyCount $ do
    i256 <- H.forAll $ Gen.filter (< maxBound) genInt256
    toInteger256 (succ i256) === succ (toInteger256 i256)

prop_pred :: Property
prop_pred =
  propertyCount $ do
    i256 <- H.forAll $ Gen.filter (> minBound) genInt256
    toInteger256 (pred i256) === pred (toInteger256 i256)

prop_toEnum_fromEnum :: Property
prop_toEnum_fromEnum =
  propertyCount $ do
    a0 <- H.forAll $ Gen.integral (Range.linear 0 (maxBound :: Int32))
    let i256 = Int256 0 0 0 (fromIntegral a0)
        e256 = fromEnum i256
    toInteger e256 === toInteger a0
    toInteger256 (toEnum e256 :: Int256) === toInteger a0

prop_addition :: Property
prop_addition =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    toInteger256 (a + b) === correctInt256 (toInteger256 a + toInteger256 b)

prop_subtraction :: Property
prop_subtraction =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    let ai = toInteger256 a
        bi = toInteger256 b
        expected = ai + (1 `shiftL` 256) - bi
    toInteger256 (a - b) === correctInt256 expected

prop_multiplication :: Property
prop_multiplication =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    toInteger256 (a * b) === correctInt256 (toInteger256 a * toInteger256 b)

prop_negate :: Property
prop_negate =
  propertyCount $ do
    i256 <- H.forAll genInt256
    toInteger256 (negate i256) === correctInt256 (negate $ toInteger256 i256)

prop_abs :: Property
prop_abs =
  propertyCount $ do
    i256 <- H.forAll genInt256
    toInteger256 (abs i256) === correctInt256 (abs $ toInteger256 i256)

prop_signum :: Property
prop_signum =
  propertyCount $ do
    i256 <- H.forAll genInt256
    toInteger256 (signum i256) === signum (toInteger256 i256)

prop_fromInteger :: Property
prop_fromInteger =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    let i256 = fromInteger $ mkInteger a3 a2 a1 a0
    (int256hi i256, int256m1 i256, int256m0 i256, int256lo i256) === (a3, a2, a1, a0)

prop_bitwise_and :: Property
prop_bitwise_and =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    toInteger256 (a .&. b) === (toInteger256 a .&. toInteger256 b)

prop_bitwise_or :: Property
prop_bitwise_or =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    toInteger256 (a .|. b) === (toInteger256 a .|. toInteger256 b)

prop_bitwise_xor :: Property
prop_bitwise_xor =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt256 <*> genInt256
    toInteger256 (xor a b) === xor (toInteger256 a) (toInteger256 b)

prop_complement :: Property
prop_complement =
  propertyCount $ do
    i256 <- H.forAll genWord256
    H.assert $ complement i256 /= i256
    complement (complement i256) === i256

prop_logical_shift_left :: Property
prop_logical_shift_left =
  propertyCount $ do
    i256 <- H.forAll genInt256
    shift <- H.forAll $ Gen.int (Range.linear 0 260)
    toInteger256 (shiftL i256 shift) === correctInt256 (shiftL (toInteger256 i256) shift)

prop_logical_shift_right :: Property
prop_logical_shift_right =
  propertyCount $ do
    i256 <- H.forAll genInt256
    shift <- H.forAll $ Gen.int (Range.linear 0 260)
    toInteger256 (shiftR i256 shift) === shiftR (toInteger256 i256) shift

prop_logical_rotate_left :: Property
prop_logical_rotate_left =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    toInteger (rotateL (Int256 a3 a2 a1 a0) rot) === correctInt256 (toInteger $ rotateL (Word256 a3 a2 a1 a0) rot)

prop_logical_rotate_right :: Property
prop_logical_rotate_right =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    toInteger (rotateR (Int256 a3 a2 a1 a0) rot) === correctInt256 (toInteger $ rotateR (Word256 a3 a2 a1 a0) rot)

prop_shift_opposite :: Property
prop_shift_opposite =
  propertyCount $ do
    i256 <- H.forAll genInt256
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    shiftL i256 rot === shiftR i256 (negate rot)

prop_testBit :: Property
prop_testBit =
  propertyCount $ do
    i256 <- H.forAll genInt256
    idx <- H.forAll $ Gen.int (Range.linearFrom 0 (-200) 200)
    let expected
          | idx < 0 = False
          | idx >= 256 = False
          | otherwise = testBit (toInteger256 i256) idx
    testBit i256 idx === expected

prop_bit :: Property
prop_bit =
  propertyCount $ do
    b <- H.forAll $ Gen.int (Range.linearFrom 0 (-300) 300)
    let idx = fromIntegral b
        expected
          | idx < 0 = 0
          | idx >= 256 = 0
          | idx == 255 = toInteger256 (minBound :: Int256)
          | otherwise = bit idx
    toInteger256 (bit idx :: Int256) === expected

prop_popCount :: Property
prop_popCount =
  propertyCount $ do
    (a3,a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    popCount (Int256 a3 a2 a1 a0) === popCount a3 + popCount a2 + popCount a1 + popCount a0

prop_countLeadingZeros :: Property
prop_countLeadingZeros =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    let expected =
          case (a3, a2, a1, a0) of
            (0, 0, 0, _) -> 192 + countLeadingZeros a0
            (0, 0, _, _) -> 128 + countLeadingZeros a1
            (0, _, _, _) -> 64 + countLeadingZeros a2
            (_, _, _, _) -> countLeadingZeros a3
    countLeadingZeros (Int256 a3 a2 a1 a0) === expected

prop_countTrailingZeros :: Property
prop_countTrailingZeros =
  propertyCount $ do
    (a3, a2, a1, a0) <- H.forAll $ (,,,) <$> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64 <*> genBiasedWord64
    let expected =
          case (a3, a2, a1, a0) of
            (_, 0, 0, 0) -> 192 + countTrailingZeros a3
            (_, _, 0, 0) -> 128 + countTrailingZeros a2
            (_, _, _, 0) -> 64 + countTrailingZeros a1
            (_, _, _, _) -> countTrailingZeros a0
    countTrailingZeros (Int256 a3 a2 a1 a0) === expected

-- Don't need to test `quot` or `rem` because they are implemented by applying
-- `fst` or `snd` to the output of `quotRem`.
prop_quotRem :: Property
prop_quotRem =
  propertyCount $ do
    num <- H.forAll genInt256
    den <- H.forAll $ Gen.filter (/= 0) genInt256
    let (q, r) = quotRem num den
    (toInteger256 q, toInteger256 r) === quotRem (toInteger256 num) (toInteger256 den)

prop_divMod :: Property
prop_divMod =
  propertyCount $ do
    num <- H.forAll genInt256
    den <- H.forAll $ Gen.filter (/= 0) genInt256
    let (d, m) = divMod num den
    (toInteger256 d, toInteger256 m) === divMod (toInteger256 num) (toInteger256 den)

prop_roundtrip_binary :: Property
prop_roundtrip_binary =
  propertyCount $ do
    i256 <- H.forAll genInt256
    H.tripping i256 Binary.encode (Just . Binary.decode)

prop_peek_and_poke :: Property
prop_peek_and_poke =
  propertyCount $ do
    i256 <- H.forAll genInt256
    ar <- liftIO $
            allocaBytes (sizeOf zeroInt256) $ \ ptr -> do
              poke ptr i256
              peek ptr
    toInteger256 ar === toInteger256 i256

prop_peekElemOff_pokeElemOff :: Property
prop_peekElemOff_pokeElemOff =
  propertyCount $ do
    a256 <- H.forAll genInt256
    b256 <- H.forAll genInt256
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroInt256) $ \ ptr -> do
                    pokeElemOff ptr 0 a256
                    pokeElemOff ptr 1 b256
                    (,) <$> peekElemOff ptr 0 <*>  peekElemOff ptr 1
    (toInteger256 ar, toInteger256 br) === (toInteger256 a256, toInteger256 b256)


prop_ToFromPrimArray :: Property
prop_ToFromPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $
      Gen.list (fromIntegral <$> (Range.linearBounded :: Range.Range Word8)) genInt256
    as === primArrayToList (primArrayFromList as)

prop_WriteReadPrimArray :: Property
prop_WriteReadPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $ Gen.list (Range.linear 1 256) genInt256
    unless (null as) $ do
      let len = length as
          arr = primArrayFromList as
      i <- (`mod` len) <$> H.forAll (Gen.int (Range.linear 0 (len - 1)))
      new <- H.forAll genInt256
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
    a256 <- H.forAll genInt256
    b256 <- H.forAll genInt256
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroInt256) $ \ ptr -> do
                    writeOffPtr ptr 0 a256
                    writeOffPtr ptr 1 b256
                    (,) <$> readOffPtr ptr 0 <*> readOffPtr ptr 1
    (ar, br) === (a256, b256)

-- -----------------------------------------------------------------------------

mkInteger :: Word64 -> Word64 -> Word64 -> Word64 -> Integer
mkInteger a3 a2 a1 a0 =
  fromIntegral a3 `shiftL` 192 +  fromIntegral a2 `shiftL` 128
    + fromIntegral a1 `shiftL` 64 + fromIntegral a0

correctInt256 :: Integer -> Integer
correctInt256 x
  | x >= minBoundInt256 && x <= maxBoundInt256 = x
  | otherwise = toInteger (fromIntegral x :: Int256)
  where
    minBoundInt256 = fromIntegral (minBound :: Int256)
    maxBoundInt256 = fromIntegral (maxBound :: Int256)

toInteger256 :: Int256 -> Integer
toInteger256 = toInteger

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
