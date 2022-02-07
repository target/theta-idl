{ pkgs }:
let
  inherit (pkgs) rust-nightly;
  theta-rust = import ./. { inherit pkgs; };

  # Extension so that racer works properly with nix develop
  #
  # Assuming we can get racer to build from nixpkgs—still broken as of
  # 2022-02-06
  rust-with-src = rust-nightly.rust.override {
    extensions = [ "rust-src" ];
  };

  rustPlatform = pkgs.makeRustPlatform {
    cargo = rust-nightly.cargo;
    rustc = rust-with-src;
  };

  rust-tools = with rustPlatform.rust; [ rustc cargo ];

  frameworks = pkgs.darwin.apple_sdk.frameworks;
  darwin = if pkgs.stdenv.isDarwin then [ frameworks.Security ] else [];
in
pkgs.mkShell {
  nativeBuildInputs = rust-tools ++ darwin;
}
