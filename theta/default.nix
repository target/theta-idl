{ pkgs

, compiler-version

, compiler ? pkgs.haskell.packages."${compiler-version}"

, overrides ? (new: old: {})

, source-overrides ? {}

, build-tools ? [               # extra tools available for nix develop
  # hack to work around stylish-haskell not building with
  # compiler = "ghc902"
  pkgs.haskellPackages.stylish-haskell

  compiler.cabal-install
  compiler.haskell-language-server
  compiler.hlint

  # for ci/stack-test
  pkgs.stack
  pkgs.jq

  # for bin/profile-compile-times
  pkgs.time-ghc-modules
]

, werror ? true

, static-executables-only ? false
}:

let
  lib = pkgs.haskell.lib;
  
  static-gmp = pkgs.gmp.override {
    stdenv = pkgs.makeStaticLibraries pkgs.stdenv;
  };

  static-deps = [
    pkgs.glibc
    pkgs.glibc.static
    pkgs.zlib.static
    static-gmp
    (pkgs.libffi.overrideDerivation (old: {
      configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
    }))
  ];

  enable-static = p: lib.overrideCabal
    (lib.justStaticExecutables p)
    ({ configureFlags ? [], extraLibraries ? [], ...}: {
      configureFlags = configureFlags ++ [ "-f" "isStatic" ];
      extraLibraries = extraLibraries ++ static-deps;
    });

  enable-werror = p:
    if werror
    then lib.appendConfigureFlag p "--ghc-option=-Werror"
    else p;

  add-build-tools = p: lib.addBuildTools p build-tools;

  excluded = [
    "dist"
    "dist-newstyle"
    "stack.yaml"
    ".stack-work"
    "stack.yaml.lock"
    "stack-shell.nix"
  ];
in
compiler.developPackage {
  name = "theta";
  root = ./.;

  inherit overrides source-overrides;

  # Don't try to build static executables on Darwin systems
  modifier = let
    base = p: enable-werror (add-build-tools p);
  in
    if pkgs.stdenv.isDarwin || (!static-executables-only)
    then base
    else p: enable-static (base p);

  # explicitly disable "smart" detection of nix-shell status
  #
  # The default value of returnShellEnv is impure: it checks the
  # value of the IN_NIX_SHELL environment variable.
  returnShellEnv = false;
}
