# This expression is meant to be built in CI.
#
# It doesn't produce any useful output, but if it builds properly,
# then the python code generation is passing tests in
# python-tests/test.py
#
# If it fails, the python error message should
# printed with the call to nix build

{ pkgs, lib }:
let
  inherit (pkgs) pythonPackages theta theta-python;

  theta-generated-python = lib.theta-python {
    name = "theta-generated-python";
    src = ./.;
    theta-paths = [./modules];
    modules = ["python_tests"];
    prefix = "theta_python_tests";
  };
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
