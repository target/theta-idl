{ pkgs

, lib

, compiler-version ? "ghc8107"

, compiler ? pkgs.haskell.packages."${compiler-version}"

, source-overrides ? {}

, overrides ? {}

, extra-build-tools ? []               # extra tools available for nix develop
}:
let
  haskell = compiler.extend (_: _: {
    inherit (pkgs) theta;
  });

  rust-executable = import ./rust { inherit pkgs lib; };

  python-executable = import ./python { inherit pkgs lib; };

  build-tools =
    [ pkgs.haskellPackages.stylish-haskell
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.hlint

      pkgs.stack
      pkgs.time-ghc-modules

      haskell.haskell-language-server

      rust-executable
      python-executable
    ] ++ extra-build-tools;

  excluded = [
    "dist"
    "dist-newstyle"
    "stack.yaml"
    ".stack-work"
    "stack.yaml.lock"
    "stack-shell.nix"
  ];

  add-build-tools = p:
    pkgs.haskell.lib.addBuildTools p build-tools;
in
haskell.developPackage {
  name = "theta-tests";
  root = ./.;

  inherit overrides source-overrides;

  modifier = add-build-tools;

  # explicitly disable "smart" detection of nix-shell status
  #
  # The default value of returnShellEnv is impure: it checks the
  # value of the IN_NIX_SHELL environment variable.
  returnShellEnv = false;
}
