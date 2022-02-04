{ pkgs ? import ../nix/nixpkgs.nix {}
, python ? pkgs.python38
}:
pkgs.mkShell {
  buildInputs = [
    python
    pkgs.poetry
  ];
}
