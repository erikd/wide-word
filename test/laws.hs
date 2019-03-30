{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.WideWord
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Classes
import Data.Semiring hiding ((+),(*))
import Data.Proxy (Proxy(Proxy))
import Data.Bits
import Foreign.Storable
import Data.Primitive.Types (Prim)

main :: IO ()
main = lawsCheckMany allPropsApplied

allPropsApplied :: [(String, [Laws])]
allPropsApplied =
  [ ("Int128", allLaws (Proxy :: Proxy Int128))
  , ("Word128", allLaws (Proxy :: Proxy Word128))
  ] 

allLaws ::
  ( Arbitrary a
  , Bits a
  , Bounded a
  , Enum a
  , Eq a
  , FiniteBits a
  , Integral a
  , Ord a
  , Prim a
  , Read a
  , Semiring a
  , Show a
  , Storable a
  ) => Proxy a -> [Laws]
allLaws p = map ($ p)
  [ bitsLaws
  , boundedEnumLaws
  , eqLaws
  , integralLaws
  , ordLaws
  , semiringLaws
  , storableLaws
  , primLaws
  ]

instance Arbitrary Word128 where
  arbitrary = Word128 <$> arbitrary <*> arbitrary

instance Arbitrary Int128 where
  arbitrary = Int128 <$> arbitrary <*> arbitrary

-- These are used to make sure that 'Num' behaves properly.
instance Semiring Word128 where
  zero = 0
  one  = 1
  plus = (+)
  times = (*)

instance Semiring Int128 where
  zero = 0
  one  = 1
  plus = (+)
  times = (*)
