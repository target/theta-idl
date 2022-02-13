{ pkgs, lib }:
let
  inherit (pkgs) pythonPackages theta theta-python;

  theta-generated-python = lib.theta-python {
    name = "test_modules";
    src = ./.;
    theta-paths = [ ../modules ];
    modules = ["everything" "primitives"];
    prefix = "cat_everything";
  };
in pythonPackages.pkgs.buildPythonPackage {
  pname = "cat_everything_python";
  version = "1.0.0";
  src = ./.;
  propagatedBuildInputs = [ theta-python pythonPackages.pkgs.hypothesis ];
  nativeBuildInputs = [ theta ];
  preConfigure = ''
    cp -r ${theta-generated-python}/* cat_everything
  '';
}
