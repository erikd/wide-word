# Revision history for wide-word

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
