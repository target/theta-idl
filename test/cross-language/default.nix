{ pkgs

, compiler-version ? "ghc8107"

, compiler ? pkgs.haskell.packages."${compiler-version}"

, extra-build-tools ? []               # extra tools available for nix develop
}:
let
  build-tools =
    [ compiler.stylish-haskell
      compiler.cabal-install
      compiler.haskell-language-server
      compiler.hlint
      pkgs.stack
      pkgs.time-ghc-modules
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
compiler.developPackage {
  name = "theta-tests";
  root = (pkgs.lib.cleanSourceWith
    {
      src = ./.;
      filter = path: type:
        !(pkgs.lib.elem (baseNameOf (toString path)) excluded)
        && !pkgs.lib.hasPrefix ".ghc.environment." (baseNameOf (toString path))
        && pkgs.lib.cleanSourceFilter path type;
    }).outPath;

  modifier = add-build-tools;

  # explicitly disable "smart" detection of nix-shell status
  #
  # The default value of returnShellEnv is impure: it checks the
  # value of the IN_NIX_SHELL environment variable.
  returnShellEnv = false;
}
