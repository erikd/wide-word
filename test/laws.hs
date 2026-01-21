{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.WideWord
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Classes
import Data.Semiring hiding ((+),(*))
import Data.Proxy (Proxy (Proxy))
import Data.Bits
import Foreign.Storable
import Data.Primitive.Types (Prim)
import Data.Maybe (catMaybes)
import Data.Word (Word64)

#if ! MIN_VERSION_base (4,11,0)
import Data.Semigroup
#endif

main :: IO ()
main = lawsCheckMany allPropsApplied

allPropsApplied :: [(String, [Laws])]
allPropsApplied =
  [ ("Int128", allLaws (Proxy :: Proxy Int128))
  , ("Word64", allLaws (Proxy :: Proxy Word64))
  , ("Word128", allLaws (Proxy :: Proxy Word128))
  , ("Int256", allLaws (Proxy :: Proxy Int256))
  , ("Word256", allLaws (Proxy :: Proxy Word256))
  ]

allLaws
  :: ( Arbitrary a
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
     , Semigroup a
     , Show a
     , Storable a
     )
  => Proxy a -> [Laws]
allLaws p =
  map ($ p)
    [ bitsLaws
    , boundedEnumLaws
    , eqLaws
    , integralLaws
    , ordLaws
    , semiringLaws
    , semigroupLaws
    , storableLaws
    , primLaws
    , numLaws
    ]

instance Arbitrary Word128 where
  arbitrary =
    Word128 <$> arbitraryBoundedIntegral <*> arbitraryBoundedIntegral

instance Arbitrary Word256 where
  arbitrary =
    Word256
      <$> arbitraryBoundedIntegral <*> arbitraryBoundedIntegral
      <*> arbitraryBoundedIntegral <*> arbitraryBoundedIntegral
  shrink x
    | x == 0 = []
    | x == 1 = [0]
    | x == 2 = [0,1]
    | x == 3 = [0,1,2]
    | otherwise =
        let y = x `shiftR` 1
            z = y + 1
            w = div (x * 9) 10
            p = div (x * 7) 8
         in catMaybes
              [ if y < x then Just y else Nothing
              , if z < x then Just z else Nothing
              , if w < x then Just w else Nothing
              , if p < x then Just p else Nothing
              ]

instance Arbitrary Int128 where
  arbitrary = Int128 <$> arbitrary <*> arbitrary

instance Arbitrary Int256 where
  arbitrary =
    Int256
      <$> arbitraryBoundedIntegral <*> arbitraryBoundedIntegral
      <*> arbitraryBoundedIntegral <*> arbitraryBoundedIntegral
  shrink x
    | x == 0 = []
    | x == 1 = [0]
    | x == 2 = [0,1]
    | x == 3 = [0,1,2]
    | otherwise =
        let y = x `shiftR` 1
            z = y + 1
            w = div (x * 9) 10
            p = div (x * 7) 8
         in catMaybes
              [ if y < x then Just y else Nothing
              , if z < x then Just z else Nothing
              , if w < x then Just w else Nothing
              , if p < x then Just p else Nothing
              ]

-- These are used to make sure that 'Num' behaves properly.
instance Semiring Word128 where
  zero = 0
  one  = 1
  plus = (+)
  times = (*)

instance Semiring Word256 where
  zero = 0
  one  = 1
  plus = (+)
  times = (*)

instance Semiring Int128 where
  zero = 0
  one  = 1
  plus = (+)
  times = (*)

instance Semiring Int256 where
  zero = 0
  one  = 1
  plus = (+)
  times = (*)

-- These are used to make sure that plus is associative
instance Semigroup Word128 where
  (<>) = (+)

instance Semigroup Word64 where
  (<>) = (+)

instance Semigroup Word256 where
  (<>) = (+)

instance Semigroup Int128 where
  (<>) = (+)

instance Semigroup Int256 where
  (<>) = (+)
