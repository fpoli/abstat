# Abstat

Abstract Interpreter for the While Language.

[![Build Status](https://travis-ci.org/fpoli/abstat.svg?branch=master)](https://travis-ci.org/fpoli/abstat)

## Quick start

1. Install the Haskell compiler `ghc` and the packet manager `cabal`
2. Install dependencies using `cabal` (e.g. `cabal install --only-dependencies --enable-tests`)
3. Run tests with `make test`
4. Run demo with `cat data/if.wl | make run`

## Example

Output of `cat data/if.wl | make run`:


```
=== Abstat ===
x := 1;

if x == 1 then (
	y := x;
	x := 4;
) else (
	z := x;
	x := 6;
)

(*) Parsing...
(*) Abstract interpretation...
( ) Int domain:
    x: Val 4
    y: Val 1
    z: Top
( ) Sign domain:
    x: Val Positive
    y: Top
    z: Top
( ) Interval domain:
    x: Interval (Val 4) (Val 4)
    y: Interval (Val 1) (Val 1)
    z: Interval MinusInf PlusInf
( ) Congruence 2:
    x: Val 0
    y: Top
    z: Top
(*) Concrete interpretation...
    x: 4
    y: 1

```

## License

Copyright (C) 2014, 2015 Federico Poli <federpoli@gmail.com>

Released under the GNU General Public License, version 3
