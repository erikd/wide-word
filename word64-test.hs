{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

#include <MachDeps.h>

import Data.Bits (shiftL, shiftR)
import Data.Word
import Data.WideWord.Compat

#if WORD_SIZE_IN_BITS == 32
import GHC.Exts (Word#, Word32#, Word64#, uncheckedShiftRL64#, word64ToWord#, wordToWord32#)
#endif

import GHC.Int (Int64 (..))
import GHC.Word (Word32 (..), Word64 (..))

import Numeric

#if WORD_SIZE_IN_BITS == 64

{-# INLINE subCarryDiff #-}
subCarryDiff :: Word64 -> Word64 -> (Word64, Word64)
subCarryDiff (W64# a) (W64# b) =
  let !(# s, c #) = subWordC# a b
   in (W64# (int2Word# c), W64# s)

testSubCarryDiff :: (Word64, Word64) -> IO ()
testSubCarryDiff (a, b) = do
    let (c, d) = subCarryDiff a b
    putStrLn $ "( x, y ) = (" ++ showHex c ", " ++ showHex d ")"
    
#elif  WORD_SIZE_IN_BITS == 32

subCarryDiff :: Word64 -> Word64 -> (Word64, Word64)
subCarryDiff (W64# a) (W64# b) =
    (mkWord64 0 (W32# (word64ToWord32# c2)), mkWord64 (W32# (word64ToWord32# d1)) (W32# (word64ToWord32# d0)))
  where
    !(# d0, c1 #) = subWordC# a b
    !(# d1a, c2a #) = subWordC# a (int2Word# c1)
    !(# d1, c2b #) = subWordC# d1a b
    !c2 = plusWord# (int2Word# c2a) (int2Word# c2b)

{-# INLINE word64ToHiWord# #-}
word64ToHiWord# :: Word64# -> Word#
word64ToHiWord# w = word64ToWord# (w `uncheckedShiftRL64#` 32#)

{-# INLINE word64ToWord32# #-}
word64ToWord32# :: Word64# -> Word32#
word64ToWord32# w =  wordToWord32# (word64ToWord# w)

{-# INLINE mkWord64 #-}
mkWord64 :: Word32 -> Word32 -> Word64
mkWord64 hi lo = fromIntegral hi `shiftL` 32 + fromIntegral lo

testSubCarryDiff :: (Word64, Word64) -> IO ()
testSubCarryDiff (W64# a, W64# b) = do
  let !(# d0, c1 #) = subWordC# a b
  putStrLn $ "(# d0, c1 #)  = (" ++ showHex (W64# d0) (", " ++ showHex (I64# c1) ")")
  let !(# d1a, c2a #) = subWordC# a (int2Word# c1)
  putStrLn $ "(# d1a, c2 #) = (" ++ showHex (W64# d1a) (", " ++ showHex (I64# c2a) ")")

  let !c2 = plusWord# (int2Word# c2a) (int2Word# c2b)

  let !(# d1, c2b #) = subWordC# d1a b
  putStrLn $ "(# d1, c2b #) = (" ++ showHex (W64# d1) (", " ++ showHex (I64# c2b) ")")
  let !c2 = plusWord# (int2Word# c2a) (int2Word# c2b)
  putStrLn $ "c2b = " ++ showHex (W64# c2) ""

  let (x, y) = (mkWord64 0 (W32# (word64ToWord32# c2)), mkWord64 (W32# (word64ToWord32# d1)) (W32# (word64ToWord32# d0)))
  putStrLn $ "( carry, diff ) = (" ++ showHex x (", " ++ showHex y ")")


--        00   00
--      - 00   01
--    -------------
--            (00, ff)
--        ff
--
#else

error "Sorry, this package only supports 32 and 64 bit word sizes."

#endif


main :: IO ()
main = 
  mapM_ testSubCarryDiff [ (0, 1) ]
