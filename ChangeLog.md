# Revision history for wide-word

## 0.1.9.0 -- 2026-01-21

* Add Int256 type contributed by Dmitry Kovalev.
* Fixed 32 bit support and CI from Bodgrim.
* Fix inclusion of MachDeps.h C header file from Bodgrim.

## 0.1.8.1 -- 2025-09-13

* Reintroduce Data instances that were incorrectly removed in 0.1.8.0.

## 0.1.8.0 -- 2025-09-08

* Fix bug in Word256 implementions (minus).
* Add property tests for Word256.

## 0.1.7.1 -- 2025-06-19

* Publish a new version removing an `if` conditional from the cabal
  file.
* Update `tested-with` versions in cabal file.

## 0.1.7.0 -- 2025-03-07

* Improvements to compare128 for Int128.

## 0.1.6.0 -- 2023-10-24

* Fixes for shifting/rotating by negative values.

## 0.1.5.0 -- 2023-01-14

* Add Binary instances for Int128, Word128 and Word256.

## 0.1.4.0 -- 2022-12-24

* Add support for building on 32 bit architectures with ghc-9.2 or later.

## 0.1.3.0 -- 2022-12-01

* Add Hashable instances for Int128, Word128, and Word256.

## 0.1.2.0 -- 2022-??-??

* Add Hashable instances for Int128, Word128, and Word256.

## 0.1.1.2 -- 2020-12-26

* Derive Generic for Int128, Word128 and Word256.
* Fix Bits.isSigned instance for Int128.

## 0.1.1.1 -- 2020-03-22

* Make `sizeOf` and `alignment` methods of `Word256` `Prim` and `Storable`
  instances agree.

## 0.1.1.0 -- 2019-11-22

* Add `Word256`.

## 0.1.0.9 -- 2019-02-06

* Fix `Prim` instance for `Int128`

## 0.1.0.8  -- 2019-01-31

* Improve implementation of succ/pred.
* Add tests for typeclass laws.
* Add Prim instances for Int128 and Word128.
* Fix/re-instate rewite rules.

## 0.1.0.7  -- 2018-11-16

* Switch to Hedgehog for testing.

## 0.1.0.3  -- 2017-04-05

* Make it build with ghc 8.2.

## 0.1.0.2  -- 2017-02-08

* Add NFData instances for Word128 and Int128.

## 0.1.0.1  -- 2017-01-29

* Int128: Fix flakey rewrite rules.

## 0.1.0.0  -- 2017-01-06

* First version. Released on an unsuspecting world.
