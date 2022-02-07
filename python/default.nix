{ pkgs }:
let
  python = pkgs.pythonPackages.pkgs;
in
python.buildPythonPackage {
  pname   = "theta-python";
  version = "1.0.2";
  src     = ./.;

  checkInputs = [ python.hypothesis ];

  # For development (nix develop)
  nativeBuildInputs = with pkgs; [ theta ];

  meta = {
    description = "The library Theta uses for parsing Avro.";
  };
}
