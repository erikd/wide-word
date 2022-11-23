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
  , or#
  , and#
  , xor#
  , timesWord#
  , plusWord#
  , word2Int#
  ) where

#if MIN_VERSION_base(4,17,0)
import qualified GHC.Base
import GHC.Prim (Word64#, wordToWord64#, word64ToWord#, Int64#)
#else
import GHC.Base (int2Word#, subWordC#, plusWord2#, or#, minusWord#, timesWord2#, word2Int#, xor#, and#, not#, plusWord#, timesWord#)
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

#endif


