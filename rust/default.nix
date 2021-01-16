{ pkgs ? import ../nix/nixpkgs.nix {} }:

pkgs.naersk.buildPackage {
  src = builtins.filterSource
    (path: type:
      type != "directory" || builtins.baseNameOf path != "target")
    ./.;

  remapPathPrefix = true;
  doCheck = true;
}
