# wide-word

[![Hackage](https://img.shields.io/hackage/v/wide-word.svg)](https://hackage.haskell.org/package/wide-word)
[![Haskell-CI](https://github.com/erikd/wide-word/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/erikd/wide-word/actions/workflows/haskell-ci.yml)

A Haskell library providing large (more than 64 bits) but fixed width signed
and unsigned integer types:

* `Word128` and `Int128`
* `Word256` and `Int256`

All types have the standard type class instances, including `Show`, `Read`,
`Eq`, `Ord`, `Ix`, `Bounded`, `Enum`, `Num`, `Bits`, `FiniteBits`, `Real`,
`Integral`, `Storable`, `Prim` (from `primitive`), `Binary`, `Hashable`,
`NFData`, `Data` and `Generic`.

The unsigned types should be drop in replacements for `Word64` (and the signed
types for `Int64`) unless you are specifically relying on the
rounding/overflow/truncation behaviour of the 64 bit types. Arithmetic is
modular, wrapping around at the type's width.

The `Data.WideWord.Word64` module also exports some helper functions for
`Word64`, such as carrying addition and multiplication.

All modules are re-exported by `Data.WideWord`.

## Building and testing

```
cabal build
cabal test
```

Tests are enabled by default in `cabal.project`. The test suites are `test64`,
`test128` and `test256` (Hedgehog property tests comparing results against
`Integer`) and `laws` (type class law checks using `quickcheck-classes`). A
single suite can be run with, for example, `cabal test test128`.

CI tests a range of GHC versions as well as 32 bit (i386) and big endian
(s390x) platforms.
