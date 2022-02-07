# This expression is meant to be built in CI.
#
# It doesn't produce any useful output, but if it builds properly,
# then the python code generation is passing tests in
# python-tests/test.py
#
# If it fails, the python error message should
# printed with the call to nix build

{ pkgs }:
let
  inherit (pkgs) pythonPackages theta theta-python;

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
  checkInputs = [ theta-python pythonPackages.pkgs.hypothesis ];
  preConfigure = ''
    cp -r ${theta-generated-python}/* theta_python_tests
  '';
}
