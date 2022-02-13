{ pkgs, lib, theta-rust-src ? ../../../rust }:
let
  inherit (pkgs) theta;

  theta-rust = builtins.filterSource
    (path: type:
      type != "directory" || builtins.baseNameOf path != "target")
    theta-rust-src;

  rust-modules = lib.theta-rust {
    name = "test_modules";
    src = ./.;
    theta-paths = [ ../modules ];
    modules = ["everything" "primitives"];
  };

  generated-rust-src = pkgs.stdenv.mkDerivation {
    name = "generated-rust-src";
    src = ./.;

    installPhase = ''
      mkdir -p $out/src

      # Theta Rust support library
      cp -r ${theta-rust} $out/theta-rust

      # Rust project files
      cp $src/Cargo.lock $out
      cp $src/Cargo.toml $out
      cp $src/src/*.rs $out/src

      # Generate Theta modules
      cp ${rust-modules}/src/*.rs $out/src
    '';
  };
in pkgs.naersk.buildPackage {
  src = generated-rust-src;

  # Without this, the theta-rust directory created in
  # theta-generated-rust (cp -r ${theta-rust} $out/theta-rust) gets
  # filtered out by Naersk.
  copySources = ["theta-rust"];
}
