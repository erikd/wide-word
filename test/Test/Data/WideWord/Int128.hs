{-# LANGUAGE TemplateHaskell #-}
module Test.Data.WideWord.Int128
  ( tests
  ) where

import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)

import           Data.Bifunctor (first)
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


-- Set the number of times to run each property test here.
propertyCount :: H.PropertyT IO () -> Property
propertyCount =
  H.withTests 10000 . H.property

prop_constructor_and_accessors :: Property
prop_constructor_and_accessors =
  propertyCount $ do
    (h, l) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    let i128 = Int128 h l
    (int128Hi64 i128, int128Lo64 i128) === (h, l)

prop_byte_swap :: Property
prop_byte_swap =
  propertyCount $ do
    h <- H.forAll genBiasedWord64
    l <- H.forAll $ Gen.filter (/= h) genBiasedWord64
    let w128 = Int128 h l
        swapped = byteSwapInt128 w128
    (byteSwapInt128 swapped, byteSwap64 (fromIntegral h), byteSwap64 (fromIntegral l))
            === (w128, int128Lo64 swapped, int128Hi64 swapped)

prop_derivied_eq_instance :: Property
prop_derivied_eq_instance =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    (b1, b0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    (Int128 a1 a0 == Int128 b1 b0) === (a1 == b1 && a0 == b0)

prop_ord_instance :: Property
prop_ord_instance =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    compare a b === compare (toInteger128 a) (toInteger128 b)

prop_show_instance :: Property
prop_show_instance =
  propertyCount $ do
    i128 <- H.forAll genInt128
    show i128 === show (toInteger128 i128)

prop_read_instance :: Property
prop_read_instance =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    read (show $ Int128 a1 a0) === Int128 a1 a0

prop_succ :: Property
prop_succ =
  propertyCount $ do
    i128 <- H.forAll genInt128
    res <- liftIO (fmap toInteger128 <$> tryEvaluate (succ i128))
    res === if i128 == maxBound
              then Left "Enum.succ{Int128}: tried to take `succ' of maxBound"
              else Right (succ $ toInteger128 i128)

prop_pred :: Property
prop_pred =
  propertyCount $ do
    i128 <- H.forAll genInt128
    res <- liftIO (fmap toInteger128 <$> tryEvaluate (pred i128))
    res === if i128 == minBound
              then Left "Enum.pred{Int128}: tried to take `pred' of minBound"
              else Right $ pred (toInteger128 i128)

tryEvaluate :: a -> IO (Either String a)
tryEvaluate x = do
  first renderException <$> try (evaluate x)
  where
    renderException :: SomeException -> String
    renderException = show

prop_toEnum_fromEnum :: Property
prop_toEnum_fromEnum =
  propertyCount $ do
    a0 <- H.forAll genWord32
    let i128 = Int128 0 (fromIntegral a0)
        e128 = fromEnum i128
    toInteger e128 === toInteger a0
    toInteger128 (toEnum e128 :: Int128) === toInteger a0

prop_addition :: Property
prop_addition =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    toInteger128 (a + b) === correctInt128 (toInteger128 a + toInteger128 b)

prop_subtraction :: Property
prop_subtraction =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    let ai = toInteger128 a
        bi = toInteger128 b
        expected = ai + (1 `shiftL` 128) - bi
    toInteger128 (a - b) === correctInt128 expected

prop_multiplication :: Property
prop_multiplication =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    toInteger128 (a * b) === correctInt128 (toInteger128 a * toInteger128 b)

prop_negate :: Property
prop_negate =
  propertyCount $ do
    i128 <- H.forAll genInt128
    toInteger128 (negate i128) === correctInt128 (negate $ toInteger128 i128)

prop_abs :: Property
prop_abs =
  propertyCount $ do
    i128 <- H.forAll genInt128
    toInteger128 (abs i128) === correctInt128 (abs $ toInteger128 i128)

prop_signum :: Property
prop_signum =
  propertyCount $ do
    i128 <- H.forAll genInt128
    toInteger128 (signum i128) === signum (toInteger128 i128)

prop_fromInteger :: Property
prop_fromInteger =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    let i128 = fromInteger $ mkInteger a1 a0
    (int128Hi64 i128, int128Lo64 i128) === (a1, a0)

prop_bitwise_and :: Property
prop_bitwise_and =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    toInteger128 (a .&. b) === (toInteger128 a .&. toInteger128 b)

prop_bitwise_or :: Property
prop_bitwise_or =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    toInteger128 (a .|. b) === (toInteger128 a .|. toInteger128 b)

prop_bitwise_xor :: Property
prop_bitwise_xor =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genInt128 <*> genInt128
    toInteger128 (xor a b) === xor (toInteger128 a) (toInteger128 b)

prop_complement :: Property
prop_complement =
  propertyCount $ do
    i128 <- H.forAll genWord128
    H.assert $ complement i128 /= i128
    complement (complement i128) === i128

prop_logical_shift_left :: Property
prop_logical_shift_left =
  propertyCount $ do
    i128 <- H.forAll genInt128
    shift <- H.forAll $ Gen.int (Range.linear 0 130)
    toInteger128 (shiftL i128 shift) === correctInt128 (shiftL (toInteger128 i128) shift)

prop_logical_shift_right :: Property
prop_logical_shift_right =
  propertyCount $ do
    i128 <- H.forAll genInt128
    shift <- H.forAll $ Gen.int (Range.linear 0 130)
    toInteger128 (shiftR i128 shift) === shiftR (toInteger128 i128) shift

prop_logical_rotate_left :: Property
prop_logical_rotate_left =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-20000) 20000)
    toInteger (rotateL (Int128 a1 a0) rot) === correctInt128 (toInteger $ rotateL (Word128 a1 a0) rot)

prop_logical_rotate_right :: Property
prop_logical_rotate_right =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-20000) 20000)
    toInteger (rotateR (Int128 a1 a0) rot) === correctInt128 (toInteger $ rotateR (Word128 a1 a0) rot)

prop_testBit :: Property
prop_testBit =
  propertyCount $ do
    i128 <- H.forAll genInt128
    idx <- H.forAll $ Gen.int (Range.linearFrom 0 (-200) 200)
    let expected
          | idx < 0 = False
          | idx >= 128 = False
          | otherwise = testBit (toInteger128 i128) idx
    testBit i128 idx === expected

prop_bit :: Property
prop_bit =
  propertyCount $ do
    b <- H.forAll $ Gen.int (Range.linearFrom 0 (-200) 200)
    let idx = fromIntegral b
        expected
          | idx < 0 = 0
          | idx >= 128 = 0
          | idx == 127 = toInteger128 (minBound :: Int128)
          | otherwise = bit idx
    toInteger128 (bit idx :: Int128) === expected

prop_popCount :: Property
prop_popCount =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    popCount (Int128 a1 a0) === popCount a1 + popCount a0

prop_countLeadingZeros :: Property
prop_countLeadingZeros =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    let expected = if a1 == 0
                    then 64 + countLeadingZeros a0
                    else countLeadingZeros a1
    countLeadingZeros (Int128 a1 a0) === expected

prop_countTrailingZeros :: Property
prop_countTrailingZeros =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genBiasedWord64 <*> genBiasedWord64
    let expected = if a0 == 0
                    then 64 + countTrailingZeros a1
                    else countTrailingZeros a0
    countTrailingZeros (Int128 a1 a0) === expected

-- Don't need to test `quot` or `rem` because they are implemented by applying
-- `fst` or `snd` to the output of `quotRem`.
prop_quotRem :: Property
prop_quotRem =
  propertyCount $ do
    num <- H.forAll genInt128
    den <- H.forAll $ Gen.filter (/= 0) genInt128
    let (q, r) = quotRem num den
    (toInteger128 q, toInteger128 r) === quotRem (toInteger128 num) (toInteger128 den)

prop_divMod :: Property
prop_divMod =
  propertyCount $ do
    num <- H.forAll genInt128
    den <- H.forAll $ Gen.filter (/= 0) genInt128
    let (d, m) = divMod num den
    (toInteger128 d, toInteger128 m) === divMod (toInteger128 num) (toInteger128 den)

prop_peek_and_poke :: Property
prop_peek_and_poke =
  propertyCount $ do
    i128 <- H.forAll genInt128
    ar <- liftIO $
            allocaBytes (sizeOf zeroInt128) $ \ ptr -> do
              poke ptr i128
              peek ptr
    toInteger128 ar === toInteger128 i128

prop_peekElemOff_pokeElemOff :: Property
prop_peekElemOff_pokeElemOff =
  propertyCount $ do
    a128 <- H.forAll genInt128
    b128 <- H.forAll genInt128
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroInt128) $ \ ptr -> do
                    pokeElemOff ptr 0 a128
                    pokeElemOff ptr 1 b128
                    (,) <$> peekElemOff ptr 0 <*>  peekElemOff ptr 1
    (toInteger128 ar, toInteger128 br) === (toInteger128 a128, toInteger128 b128)

prop_ToFromPrimArray :: Property
prop_ToFromPrimArray =
  propertyCount $ do
    as <- H.forAll $
      Gen.list (fromIntegral <$> (Range.linearBounded :: Range.Range Word8)) genInt128
    as === primArrayToList (primArrayFromList as)

prop_WriteReadPrimArray :: Property
prop_WriteReadPrimArray =
  propertyCount $ do
    as <- H.forAll $ Gen.list (Range.linear 1 256) genInt128
    unless (null as) $ do
      let len = length as
          arr = primArrayFromList as
      i <- (`mod` len) <$> H.forAll (Gen.int (Range.linear 0 (len - 1)))
      new <- H.forAll genInt128
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
    a128 <- H.forAll genInt128
    b128 <- H.forAll genInt128
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroInt128) $ \ ptr -> do
                    writeOffPtr ptr 0 a128
                    writeOffPtr ptr 1 b128
                    (,) <$> readOffPtr ptr 0 <*> readOffPtr ptr 1
    (ar, br) === (a128, b128)

-- -----------------------------------------------------------------------------

mkInteger :: Word64 -> Word64 -> Integer
mkInteger a1 a0 = fromIntegral a1 `shiftL` 64 + fromIntegral a0

-- Convert an `Integer` to the `Integer` with the same bit pattern as the
-- corresponding `Int128`.
correctInt128 :: Integer -> Integer
correctInt128 x
  | x >= minBoundInt128 && x <= maxBoundInt128 = x
  | otherwise = toInteger (fromIntegral x :: Int128)
  where
    minBoundInt128 = fromIntegral (minBound :: Int128)
    maxBoundInt128 = fromIntegral (maxBound :: Int128)

toInteger128 :: Int128 -> Integer
toInteger128 = toInteger

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
