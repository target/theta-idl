{ pkgs }:
{ modules
, theta-paths
, prefix ? null
, ...
} @ attrs:
let
  inherit (pkgs.lib) strings;

  to-remove = [ "modules" "theta-paths" "prefix" "buildInputs" ];

  # Convert a list of directories to a Theta load path, with each
  # directory starting in "$src".
  THETA_LOAD_PATH = strings.concatStringsSep ":" theta-paths;

  module-flags = strings.concatStringsSep " " (map (name: "-m ${name}") modules);

  prefix-flag = if prefix == null then "" else "--prefix ${prefix}";
in pkgs.stdenv.mkDerivation (builtins.removeAttrs attrs to-remove // {
  buildInputs = (attrs.buildInputs or []) ++ [ pkgs.theta ];

  installPhase = ''
    export THETA_LOAD_PATH="${THETA_LOAD_PATH}"

    # Genrate Python code for the listed Theta modules
    mkdir -p $out
    theta python ${module-flags} -o $out ${prefix-flag}
  '';
})
