{-# LANGUAGE TemplateHaskell #-}
module Test.Data.WideWord.Word128
  ( tests
  ) where

import           Control.Exception (ArithException, SomeException, evaluate, try)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless)

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
    (h, l) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    let w128 = Word128 h l
    (word128Hi64 w128, word128Lo64 w128) === (h, l)

prop_byte_swap :: Property
prop_byte_swap =
  propertyCount $ do
    h <- H.forAll genWord64
    l <- H.forAll $ Gen.filter (/= h) genWord64
    let w128 = Word128 h l
        swapped = byteSwapWord128 w128
    (byteSwapWord128 swapped, byteSwap64 h, byteSwap64 l)
            === (w128, word128Lo64 swapped, word128Hi64 swapped)

prop_derivied_eq_instance :: Property
prop_derivied_eq_instance =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    (b1, b0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    (Word128 a1 a0 == Word128 b1 b0) === (a1 == b1 && a0 == b0)

prop_ord_instance :: Property
prop_ord_instance =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    compare a b === compare (toInteger128 a) (toInteger128 b)

prop_show_instance :: Property
prop_show_instance =
  propertyCount $ do
    w128 <- H.forAll genWord128
    show w128 === show (toInteger128 w128)

prop_read_instance :: Property
prop_read_instance =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    read (show $ Word128 a1 a0) === Word128 a1 a0

prop_read_show :: Property
prop_read_show =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    H.tripping (Word128 a1 a0) show (Just . read)

prop_succ :: Property
prop_succ =
  propertyCount $ do
    w128 <- H.forAll genWord128
    res <- liftIO (fmap toInteger128 <$> tryEvaluate (succ w128))
    res === if w128 == maxBound
              then Left "Enum.succ{Word128}: tried to take `succ' of maxBound"
              else Right (succ $ toInteger128 w128)

prop_pred :: Property
prop_pred =
  propertyCount $ do
    w128 <- H.forAll genWord128
    res <- liftIO (fmap toInteger128 <$> tryEvaluate (pred w128))
    res === if w128 == 0
              then Left "Enum.pred{Word128}: tried to take `pred' of minBound"
              else Right $ pred (toInteger128 w128)

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
    let w128 = Word128 0 (fromIntegral a0)
        e128 = fromEnum w128
    toInteger e128 === toInteger a0
    toInteger128 (toEnum e128 :: Word128) === toInteger a0

prop_addition :: Property
prop_addition =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    toInteger128 (a + b) === correctWord128 (toInteger128 a + toInteger128 b)

prop_subtraction :: Property
prop_subtraction =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    let ai = toInteger128 a
        bi = toInteger128 b
        expected = ai + (1 `shiftL` 128) - bi
    toInteger128 (a - b) === correctWord128 expected

prop_multiplication :: Property
prop_multiplication =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    toInteger128 (a * b) === correctWord128 (toInteger128 a * toInteger128 b)

prop_negate :: Property
prop_negate =
  propertyCount $ do
    w128 <- H.forAll genWord128
    toInteger128 (negate w128) === correctWord128 (negate $ toInteger128 w128)

prop_abs :: Property
prop_abs =
  propertyCount $ do
    w128 <- H.forAll genWord128
    toInteger128 (abs w128) === correctWord128 (abs $ toInteger128 w128)

prop_signum :: Property
prop_signum =
  propertyCount $ do
    w128 <- H.forAll genWord128
    toInteger128 (signum w128) === signum (toInteger128 w128)

prop_fromInteger :: Property
prop_fromInteger =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    let w128 = fromInteger $ mkInteger a1 a0
    (word128Hi64 w128, word128Lo64 w128) === (a1, a0)

prop_bitwise_and :: Property
prop_bitwise_and =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    toInteger128 (a .&. b) === (toInteger128 a .&. toInteger128 b)

prop_bitwise_or :: Property
prop_bitwise_or =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    toInteger128 (a .|. b) === (toInteger128 a .|. toInteger128 b)

prop_bitwise_xor :: Property
prop_bitwise_xor =
  propertyCount $ do
    (a, b) <- H.forAll $ (,) <$> genWord128 <*> genWord128
    toInteger128 (xor a b) === xor (toInteger128 a) (toInteger128 b)

prop_complement :: Property
prop_complement =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    toInteger128 (complement $ Word128 a1 a0) === mkInteger (complement a1) (complement a0)

prop_logical_shift_left :: Property
prop_logical_shift_left =
  propertyCount $ do
    w128 <- H.forAll genWord128
    shift <- H.forAll $ Gen.int (Range.linear 0 130)
    toInteger128 (shiftL w128 shift) === correctWord128 (shiftL (toInteger128 w128) shift)

prop_logical_shift_right :: Property
prop_logical_shift_right =
  propertyCount $ do
    w128 <- H.forAll genWord128
    shift <- H.forAll $ Gen.int (Range.linear 0 130)
    toInteger128 (shiftR w128 shift) === shiftR (toInteger128 w128) shift

prop_logical_rotate_left :: Property
prop_logical_rotate_left =
  propertyCount $ do
    w128 <- H.forAll genWord128
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-20000) 20000)
    let i128 = toInteger128 w128
        expected
          | rot < 0 = 0
          | otherwise =
              correctWord128 (i128 `shiftL` erot + i128 `shiftR` (128 - (erot `mod` 128)))
              where
                erot
                  | rot < 0 = 128 - (abs rot `mod` 128)
                  | otherwise = rot `mod` 128
    toInteger128 (rotateL w128 rot) === expected

prop_logical_rotate_right :: Property
prop_logical_rotate_right =
  propertyCount $ do
    w128 <- H.forAll genWord128
    rot <- H.forAll $ Gen.int (Range.linearFrom 0 (-20000) 20000)
    let i128 = toInteger128 w128
        expected =
          correctWord128 $ i128 `shiftR` erot + i128 `shiftL` (128 - erot)
          where
            erot
              | rot < 0 = 128 - (abs rot `mod` 128)
              | otherwise = rot `mod` 128
    toInteger128 (rotateR w128 rot) === expected

prop_testBit :: Property
prop_testBit =
  propertyCount $ do
    w128 <- H.forAll genWord128
    idx <- H.forAll $ Gen.int (Range.linearFrom 0 (-200) 200)
    let expected
          | idx < 0 = False
          | idx >= 128 = False
          | otherwise = testBit (toInteger128 w128) idx
    testBit w128 idx === expected

prop_bit :: Property
prop_bit =
  propertyCount $ do
    b <- H.forAll $ Gen.int (Range.linearFrom 0 (-200) 200)
    let idx = fromIntegral b
        expected
          | idx < 0 = 0
          | idx >= 128 = 0
          | otherwise = bit idx
    toInteger128 (bit idx :: Word128) === expected

prop_popCount :: Property
prop_popCount =
  propertyCount $ do
    w128 <- H.forAll genWord128
    popCount w128 === popCount (toInteger128 w128)

prop_countLeadingZeros :: Property
prop_countLeadingZeros =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    let expected = if a1 == 0
                    then 64 + countLeadingZeros a0
                    else countLeadingZeros a1
    countLeadingZeros (Word128 a1 a0) === expected

prop_countTrailingZeros :: Property
prop_countTrailingZeros =
  propertyCount $ do
    (a1, a0) <- H.forAll $ (,) <$> genWord64 <*> genWord64
    let expected = if a0 == 0
                    then 64 + countTrailingZeros a1
                    else countTrailingZeros a0
    countTrailingZeros (Word128 a1 a0) === expected

-- Don't need to test `quot` or `rem` because they are implemented by applying
-- `fst` or `snd` to the output of `quotRem`.
prop_quotRem :: Property
prop_quotRem =
  propertyCount $ do
    num <- H.forAll genWord128
    den <- H.forAll $ Gen.filter (/= 0) genWord128
    let (q, r) = quotRem num den
    (toInteger128 q, toInteger128 r) === quotRem (toInteger128 num) (toInteger128 den)

prop_divMod :: Property
prop_divMod =
  propertyCount $ do
    num <- H.forAll genWord128
    den <- H.forAll $ Gen.filter (/= 0) genWord128
    let (d, m) = divMod num den
    (toInteger128 d, toInteger128 m) === divMod (toInteger128 num) (toInteger128 den)

prop_peek_and_poke :: Property
prop_peek_and_poke =
  propertyCount $ do
    w128 <- H.forAll genWord128
    ar <- liftIO $
            allocaBytes (sizeOf zeroWord128) $ \ ptr -> do
              poke ptr w128
              peek ptr
    toInteger128 ar === toInteger128 w128

prop_peekElemOff_pokeElemOff :: Property
prop_peekElemOff_pokeElemOff =
  propertyCount $ do
    a128 <- H.forAll genWord128
    b128 <- H.forAll genWord128
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroWord128) $ \ ptr -> do
                    pokeElemOff ptr 0 a128
                    pokeElemOff ptr 1 b128
                    (,) <$> peekElemOff ptr 0 <*>  peekElemOff ptr 1
    (toInteger128 ar, toInteger128 br) === (toInteger128 a128, toInteger128 b128)


prop_ToFromPrimArray :: Property
prop_ToFromPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $
      Gen.list (fromIntegral <$> (Range.linearBounded :: Range.Range Word8)) genWord128
    as === primArrayToList (primArrayFromList as)


prop_WriteReadPrimArray :: Property
prop_WriteReadPrimArray =
  H.withTests 2000 . H.property $ do
    as <- H.forAll $ Gen.list (Range.linear 1 256) genWord128
    unless (null as) $ do
      let len = length as
          arr = primArrayFromList as
      i <- (`mod` len) <$> H.forAll (Gen.int (Range.linear 0 (len - 1)))
      new <- H.forAll genWord128
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
    a128 <- H.forAll genWord128
    b128 <- H.forAll genWord128
    (ar, br) <- liftIO $
                  allocaBytes (2 * sizeOf zeroWord128) $ \ ptr -> do
                    writeOffPtr ptr 0 a128
                    writeOffPtr ptr 1 b128
                    (,) <$> readOffPtr ptr 0 <*> readOffPtr ptr 1
    (ar, br) === (a128, b128)


-- -----------------------------------------------------------------------------

mkInteger :: Word64 -> Word64 -> Integer
mkInteger a1 a0 = fromIntegral a1 `shiftL` 64 + fromIntegral a0

correctWord128 :: Integer -> Integer
correctWord128 i
  | i >= 0 && i <= maxWord128 = i
  | otherwise = i .&. maxWord128
  where
    maxWord128 = (1 `shiftL` 128) - 1

toInteger128 :: Word128 -> Integer
toInteger128 = toInteger

showArithException :: ArithException -> String
showArithException = show

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
