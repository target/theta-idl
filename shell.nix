{ pkgs ? import nix/nixpkgs.nix {}
, theta ? import ./theta {}
, build-rust-target ? true
}:

let

  nightly-channel = pkgs.rustChannelOf {
    channel = "nightly";
    date = "2019-06-17";
  };

  # Add the rust sources so that racer works properly in the nix shell (if we
  # can get it to compile...)
  rust-with-src =
    nightly-channel.rust.override {
      extensions = ["rust-src"];
    };

  rustPlatform =
    pkgs.makeRustPlatform {
      cargo = nightly-channel.cargo;
      rustc = rust-with-src;
    };

  frameworks = pkgs.darwin.apple_sdk.frameworks;
  darwin-stuff = if pkgs.stdenv.isDarwin then [ frameworks.Security ] else [];
  rust-stuff = if build-rust-target then [rustPlatform.rust.cargo rustPlatform.rust.rustc] else [];
  buildInputs = with pkgs.haskellPackages;
    [ niv cabal-install stylish-haskell ] ++ darwin-stuff ++ rust-stuff;

in
# upstream developPackage does not pull in cabal-install
pkgs.lib.overrideDerivation theta.env (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ buildInputs;
})
