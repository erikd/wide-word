{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module exists to centralize compatibility shims for GHC 9.4.
module Data.WideWord.Compat
  ( plusWord2#
  , timesWord2#
  , int2Word#
  , minusWord#
  , subWordC#
  , not#
  , isZeroWord#
  , or#
  , and#
  , xor#
  , timesWord#
  , plusWord#
  , word2Int#
  , quotRemWord2#
  , compatWordLiteral#
  , compatIntLiteral#
  , compatCaseOnWordLiteral#
  , compatCaseOnIntLiteral#
  ) where

#if MIN_VERSION_base(4,17,0)
import qualified GHC.Base
import GHC.Prim (Word64#, wordToWord64#, word64ToWord#, Int64#)
#else
import qualified GHC.Base
import GHC.Base (Int#(..), Word#, quotRemWord2#, int2Word#, subWordC#, plusWord2#, or#, minusWord#, timesWord2#, word2Int#, xor#, and#, not#, plusWord#, timesWord#)
#endif

#if MIN_VERSION_base(4,17,0)
plusWord2# :: Word64# -> Word64# -> (# Word64#, Word64# #)
plusWord2# a b =
 case GHC.Base.plusWord2# (word64ToWord# a) (word64ToWord# b) of
   (# a', b' #) ->
     (# wordToWord64# a', wordToWord64# b' #)

timesWord2# :: Word64# -> Word64# -> (# Word64#, Word64# #)
timesWord2# a b =
  case GHC.Base.timesWord2# (word64ToWord# a) (word64ToWord# b) of
    (# a', b' #) ->
     (# wordToWord64# a', wordToWord64# b' #)

int2Word# :: Int64# -> Word64#
int2Word# = GHC.Base.int64ToWord64#

minusWord# :: Word64# -> Word64# -> Word64#
minusWord# a b =
  wordToWord64# (GHC.Base.minusWord# (word64ToWord# a) (word64ToWord# b))

subWordC# :: Word64# -> Word64# -> (# Word64#, Int64# #)
subWordC# a b =
  case GHC.Base.subWordC# (word64ToWord# a) (word64ToWord# b) of
    (# a', b' #) ->
     (# wordToWord64# a', GHC.Base.intToInt64# b' #)

not# :: Word64# -> Word64#
not# = GHC.Base.not64#

or# :: Word64# -> Word64# -> Word64#
or# = GHC.Base.or64#

xor# :: Word64# -> Word64# -> Word64#
xor# = GHC.Base.xor64#

and# :: Word64# -> Word64# -> Word64#
and# = GHC.Base.and64#

timesWord# :: Word64# -> Word64# -> Word64#
timesWord# = GHC.Base.timesWord64#

plusWord# :: Word64# -> Word64# -> Word64#
plusWord# = GHC.Base.plusWord64#

word2Int# :: Word64# -> Int64#
word2Int# = GHC.Base.word64ToInt64#

quotRemWord2# :: Word64# -> Word64# -> Word64# -> (# Word64#, Word64# #)
quotRemWord2# a b c =
    case GHC.Base.quotRemWord2# (word64ToWord# a) (word64ToWord# b) (word64ToWord# c) of
        (# x, y #) ->
            (# wordToWord64# x, wordToWord64# y #)
#endif

isZeroWord#
#if MIN_VERSION_base(4,17,0)
    :: Word64# -> Bool
isZeroWord# a = GHC.Base.isTrue# (GHC.Base.eqWord# (word64ToWord# a) 0##)
#else
    :: Word# -> Bool
isZeroWord# 0## = True
#endif

compatWordLiteral#
#if MIN_VERSION_base(4,17,0)
  :: GHC.Base.Word# -> Word64#
compatWordLiteral# = wordToWord64#
#else
  :: Word# -> Word#
compatWordLiteral# a = a
#endif

compatIntLiteral#
#if MIN_VERSION_base(4,17,0)
    :: GHC.Base.Int# -> Int64#
compatIntLiteral# = GHC.Base.intToInt64#
#else
    :: Int# -> Int#
compatIntLiteral# a = a
#endif

compatCaseOnWordLiteral#
#if MIN_VERSION_base(4,17,0)
    :: Word64# -> GHC.Base.Word#
compatCaseOnWordLiteral# = word64ToWord#
#else
    :: Word# -> Word#
compatCaseOnWordLiteral# a = a
#endif

compatCaseOnIntLiteral#
#if MIN_VERSION_base(4,17,0)
    :: Int64# -> GHC.Base.Int#
compatCaseOnIntLiteral# = GHC.Base.int64ToInt#
#else
    :: Int# -> Int#
compatCaseOnIntLiteral# a = a
#endif

