{
  description = "Theta is a tool for sharing algebraic data types between different languages. You can write a schema once, generate friendly bindings in Haskell, Python and Rust then share data between programs with Avro.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    naersk-flake.url = "github:nix-community/naersk";
    rust-overlay.url = "github:mozilla/nixpkgs-mozilla";
  };

  outputs = { self, nixpkgs, flake-utils, naersk-flake, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        theta = import ./theta { inherit pkgs; };
        rust = import ./rust { inherit pkgs; };
        python = import ./python { inherit pkgs; };
        test = import ./test { inherit pkgs; };

        theta-overlay = final: current: {
          inherit theta;
          theta-rust = rust;
          theta-python = python;
        };

        overlays = [
          theta-overlay
          rust-overlay.overlay
          (import nix/overlays/rust.nix { inherit naersk-flake; })
          (import nix/overlays/python.nix { python-version = "3.8"; })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        inherit overlays;

        packages = {
          inherit theta rust python test;
        };

        devShells = {
          theta = packages.theta.env;
          rust = pkgs.mkShell {
            nativeBuildInputs = pkgs.rust-dev-tools;
          };
          test = import ./test/shell.nix { inherit pkgs; };
        };

        defaultPackage = packages.theta;
      });
}
