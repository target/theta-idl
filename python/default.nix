{ pkgs ? import ../nix/nixpkgs.nix {}
, python ? pkgs.python38
}:
let
  pythonPackages = python.override {
    packageOverrides = self: super: {
      hypothesis = import ./hypothesis.nix { inherit python; };
    };
  };
in
pythonPackages.pkgs.buildPythonPackage {
  pname   = "theta-python";
  version = "1.0.2";
  src     = ./.;

  checkInputs = [ pythonPackages.pkgs.hypothesis ];

  meta = {
    description = "The library Theta uses for parsing Avro.";
  };
}
