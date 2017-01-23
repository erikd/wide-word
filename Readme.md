# wide-word

A Haskell library providing `Word128` and `Int128` data types with all the
standard type class instances including `Show`, `Read`, `Ord`, `Bounded`,
`Enum`, `Num`, `Bits`, `FiniteBits`, `Real`, `Integral` and `Storable`.

The `Word128` type should be a drop in replacement for `Word64` unless you are
specifically relying on the rounding/overflow/truncation behaviour of `Word64`.

