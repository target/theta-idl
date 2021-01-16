# Contributing

### Issues

Please feel free to submit bug reports, questions and feature requests as issues.

### How to Contribute

  1. Fork Theta and make your changes in a branch.
  2. Open and submit a pull request.
  
Take a look at the instructions in the [README] for how to use `nix` and `nix-shell` to work on different components of Theta.

### Making Contributions

Feel free to open a pull request before your contribution is ready—an open PR makes it easier for others to comment on and help with your code. Please mark pull requests that aren't ready to merge as drafts.

Before a pull request is merged, it will need to meet a few requirements:

  * Haskell code should follow the same style as the rest of the codebase. Please use [stylish-haskell] (provided in the main `nix-shell`) with its default config to format each Haskell file.
  * Identifiers exported from any Haskell module should have Haddock comments. Feel free to add documentation comments to unexported definitions too!
  * Please include unit tests for any new functionality.
  * Tests pass and the Haskell code builds without warnings—you can check this locally with `nix-build test`.
  
[stylish-haskell]: https://github.com/jaspervdj/stylish-haskell
