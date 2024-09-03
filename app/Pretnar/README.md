# Implementation of Effect Handling from Pretnar (2015)

Based on the following paper: https://www.eff-lang.org/handlers-tutorial.pdf

`Lang.hs` contains the implementation, and `Example.hs` contains:
- a "meta-handler" for specific unhandled effects (such as `print`ing to the console via Haskell I/O)
- implementations of test programs from the paper
- a `test` function that typechecks and evaluates a given computation

## Variations from Literature
- extended the language with strings, units, and tuples
  - added a `Join` primitive computation to join strings
  - added `First` and `Second` primitive computations to destructure tuples
- added type annotations to functions and handlers
  - required for typechecking without polymorphism/inference
- renamed for clarity:
  - `Op` -> `Perform`
  - `Do` -> `Let`