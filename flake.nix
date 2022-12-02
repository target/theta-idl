{
  description = "Theta is a tool for sharing algebraic data types between different languages. You can write a schema once, generate friendly bindings in Haskell, Python and Rust then share data between programs with Avro.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    naersk-flake.url = "github:nix-community/naersk";
    rust-overlay.url = "github:mozilla/nixpkgs-mozilla";

    # HACK: nix automatically unpacks this, but we will need to tar.gz
    # it again to override all-cabal-hashes :/
    all-cabal-hashes-unpacked = {
      flake = false;
      url = "github:commercialhaskell/all-cabal-hashes/current-hackage";
    };
  };
  outputs = { self, nixpkgs, flake-utils, naersk-flake, rust-overlay, all-cabal-hashes-unpacked }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        source-overrides = {
          aeson = "2.0.3.0";
          aeson-pretty = "0.8.9";
          attoparsec = "0.14.4";
          avro = "0.6.1.2";
          HasBigDecimal = "0.2.0.0";
          hashable = "1.4.0.2";
          OneTuple = "0.3.1";
          PyF = "0.11.1.0";
          quickcheck-instances = "0.3.28";
          semialign = "1.2.0.1";
          stache = "2.3.1";
          streamly = "0.8.1.1";
          streamly-bytestring = "0.1.4";
          streamly-process = "0.2.0";
          text-short = "0.1.5";
          time-compat = "1.9.6.1";
          unordered-containers = "0.2.19.1";
          versions = "5.0.2";
        };

        overrides = new: old: {
          foldl = pkgs.haskell.lib.doJailbreak old.foldl;
          streamly-process = pkgs.haskell.lib.dontCheck old.streamly-process;

          # really slow test suite:
          ListLike = pkgs.haskell.lib.dontCheck old.ListLike;

          # test failing for what seems to be a Nix-specific reason
          streamly-bytestring = pkgs.haskell.lib.dontCheck old.streamly-bytestring;
        };

        theta_8107 = import ./theta {
          inherit pkgs source-overrides overrides;
          compiler-version = "ghc8107";
        };
        theta_902 = import ./theta {
          inherit pkgs source-overrides overrides;
          compiler-version = "ghc902";
        };
        theta_925 = import ./theta {
          inherit pkgs source-overrides overrides;
          compiler-version = "ghc925";
        };

        # default is 8.10.7 (for now?)
        theta = theta_8107;

        rust = import ./rust { inherit pkgs; };
        python = import ./python { inherit pkgs; };
        test = import ./test { inherit pkgs lib source-overrides overrides; };

        theta-overlay = final: current: {
          inherit theta;
          theta-rust = rust;
          theta-python = python;
        };

        haskell-overlay = final: current: {
          # HACK: explicitly repacking all-cabal-hashes so that it
          # works with callHackage
          all-cabal-hashes = current.runCommand "all-cabal-hashes.tar.gz" {} ''
            cd ${all-cabal-hashes-unpacked}
            cd ..
            tar czf $out $(basename ${all-cabal-hashes-unpacked})
          '';
        };

        overlays = [
          theta-overlay
          haskell-overlay
          rust-overlay.overlay
          (import nix/overlays/rust.nix { inherit naersk-flake; })
          (import nix/overlays/python.nix { python-version = "3.8"; })
        ];

        pkgs = import nixpkgs { inherit system overlays; };

        lib = {
          theta-rust = import nix/lib/theta-rust.nix { inherit pkgs; };
          theta-python = import nix/lib/theta-python.nix { inherit pkgs; };
        };
      in rec {
        inherit overlays lib;

        packages = {
          inherit theta theta_8107 theta_902 theta_925 rust python test;
        };

        devShells = {
          theta = packages.theta.env;
          theta_8107 = packages.theta_8107.env;
          theta_902 = packages.theta_902.env;
          theta_925 = packages.theta_925.env;
          rust = pkgs.mkShell {
            nativeBuildInputs = pkgs.rust-dev-tools;
          };
          test = import ./test/shell.nix { inherit pkgs lib; };
        };

        defaultPackage = packages.theta;
      });
}
