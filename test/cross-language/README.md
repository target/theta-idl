# Cross-Language Tests

This directory sets up cross-language tests, checking that values round-trip through each pair of supported languages:

  * Haskell
  * Rust
  * Python

The tests are coordinated through a Haskell project (`theta-tests.cabal`), checking against code generated in Python and Rust.
