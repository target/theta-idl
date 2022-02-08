{ pkgs

, compiler-version ? "ghc8107"

, compiler ? pkgs.haskell.packages."${compiler-version}"

, source-overrides ? {
  aeson = "2.0.3.0";
  aeson-pretty = "0.8.9";
  attoparsec = "0.14.4";
  avro = "0.6.0.1";
  hashable = "1.4.0.2";
  OneTuple = "0.3.1";
  quickcheck-instances = "0.3.27";
  semialign = "1.2.0.1";
  stache = "2.3.1";
  text-short = "0.1.5";
  time-compat = "1.9.6.1";
  unordered-containers = "0.2.16.0";
  versions = "5.0.2";
}

, build-tools ? [               # extra tools available for nix develop
  compiler.stylish-haskell
  compiler.cabal-install
  compiler.haskell-language-server
  compiler.hlint
  pkgs.stack
  pkgs.time-ghc-modules
]

, werror ? true
}:

let
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

  enable-static = p: pkgs.haskell.lib.overrideCabal
    (pkgs.haskell.lib.justStaticExecutables p)
    ({ configureFlags ? [], extraLibraries ? [], ...}: {
      configureFlags = configureFlags ++ [ "-f" "isStatic" ];
      extraLibraries = extraLibraries ++ static-deps;
    });

  enable-werror = p:
    if werror
    then pkgs.haskell.lib.appendConfigureFlag p "--ghc-option=-Werror"
    else p;

  add-build-tools = p:
    pkgs.haskell.lib.addBuildTools p build-tools;

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
  root = (pkgs.lib.cleanSourceWith
    {
      src = ./.;
      filter = path: type:
           !(pkgs.lib.elem (baseNameOf (toString path)) excluded)
        && !pkgs.lib.hasPrefix ".ghc.environment." (baseNameOf (toString path))
        && pkgs.lib.cleanSourceFilter path type;
    }).outPath;

  inherit source-overrides;
  overrides = new: old: {
    foldl = pkgs.haskell.lib.doJailbreak old.foldl;
  };

  # Don't try to build static executables on Darwin systems
  modifier = let
    base = p: enable-werror (add-build-tools p);
  in
    if pkgs.stdenv.isDarwin then base else p: enable-static (base p);

  # explicitly disable "smart" detection of nix-shell status
  #
  # The default value of returnShellEnv is impure: it checks the
  # value of the IN_NIX_SHELL environment variable.
  returnShellEnv = false;
}
