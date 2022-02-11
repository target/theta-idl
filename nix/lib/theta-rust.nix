{ pkgs }:
{ modules
, theta-paths
, ...
} @ attrs:
let
  inherit (pkgs.lib) strings;

  to-remove = [ "modules" "theta-paths" "buildInputs" ];

  # Convert a list of directories to a Theta load path, with each
  # directory starting in "$src".
  THETA_LOAD_PATH = strings.concatStringsSep ":" theta-paths;

  module-flags = strings.concatStringsSep " " (map (name: "-m ${name}") modules);
in
pkgs.stdenv.mkDerivation (builtins.removeAttrs attrs to-remove // {
  buildInputs = (attrs.buildInputs or []) ++ [ pkgs.theta ];

  installPhase = ''
    export THETA_LOAD_PATH="${THETA_LOAD_PATH}"

    # Generate Rust code for the listed Theta modules
    mkdir -p $out/src
    theta rust ${module-flags} > $out/src/$name.rs
  '';
})
