# This expression is meant to be built in CI.
#
# It doesn't produce any useful output, but if it builds properly,
# then the python code generation is passing tests in
# python-tests/test.py
#
# If it fails, the python error message should
# printed with the call to nix build

{ pkgs ? import ../../nix/nixpkgs.nix {}
, python ? pkgs.python37
, theta ? import ../../theta { inherit pkgs; }
}:
let
  pythonPackages =
    python.override {
      packageOverrides = self: super: {
        hypothesis = import ../../python/hypothesis.nix { inherit pkgs python; };

        # The support package Theta-generated code uses to parse Avro.
        theta-python = import ../../python { inherit pkgs python; };
      };
    };

  theta-generated-python = pkgs.runCommand "theta-generated-python" {
    THETA_LOAD_PATH = ./modules;
    THETA = "${theta}/bin/theta";
  } ''
      mkdir -p $out
      $THETA python -m python_tests -o $out --prefix theta_python_tests
    '';
in pythonPackages.pkgs.buildPythonPackage {
  pname = "theta-python-tests";
  version = "1.0.0";
  src = ./.;
  nativeBuildInputs = [ theta ];
  checkInputs = with pythonPackages.pkgs; [ theta-python hypothesis ];
  preConfigure = ''
    cp -r ${theta-generated-python}/* theta_python_tests
  '';
}
