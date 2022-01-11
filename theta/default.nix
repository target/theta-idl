{ compiler-version ? "ghc884"
, sources ? import ../nix/sources.nix
, pkgs ? import ../nix/nixpkgs.nix { inherit sources;}
, compiler ? pkgs.haskell.packages."${compiler-version}"
, source-overrides ? {
  avro = "0.5.2.0";
  string-interpolate = "0.2.1.0";
}
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

  expose-cabal = p:
    pkgs.haskell.lib.addBuildTool p compiler.cabal-install;

in
compiler.developPackage {
  name = "theta";
  root = (pkgs.lib.cleanSourceWith
    {
      src = ./.;
      filter = path: type:
           !(baseNameOf (toString path) == "dist-newstyle")
        && !pkgs.lib.hasPrefix ".ghc.environment." (baseNameOf (toString path))
        && pkgs.lib.cleanSourceFilter path type;
    }).outPath;

  inherit source-overrides;
  overrides = new: old: {
    language-rust = pkgs.haskell.lib.dontCheck
      (new.callCabal2nix "language-rust" sources.language-rust {});
  };

  # Don't try to build static executables on Darwin systems
  modifier = let
    base = p: enable-werror (expose-cabal p);
  in
    if pkgs.stdenv.isDarwin then base else p: enable-static (base p);

  # explicitly disable "smart" detection of nix-shell status
  #
  # The default value of returnShellEnv is impure: it checks the
  # value of the IN_NIX_SHELL environment variable.
  returnShellEnv = false;
}
