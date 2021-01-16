args@{ sources ? import ./sources.nix
     , nixpkgs ? sources.nixpkgs
     }:

let
  clean-args = builtins.removeAttrs args [ "nixpkgs" "sources" ];

  mozilla-overlay = import (sources.nixpkgs-mozilla + "/rust-overlay.nix");
                           
  haskell-overlay = final: current: {
    all-cabal-hashes = sources.all-cabal-hashes;
  };

  my-overlay = final: current: {
    rust-nightly = final.rustChannelOf {
      channel = "nightly";
      date = "2020-08-27";
      sha256 = "0d9bna9l8w7sps7hqjq35835p2pp73dvy3y367b0z3wg1ha7gvjj";
    };

    naersk = current.callPackage sources.naersk {
      rustc = final.rust-nightly.rust;
      cargo = final.rust-nightly.rust;
    };
  };

  overlays = [ mozilla-overlay
               haskell-overlay
               my-overlay
             ] ++ (clean-args.overlays or []);

in import nixpkgs (clean-args // { inherit overlays; })
