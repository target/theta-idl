# Overlay setting up the right version of Python to use.
{ python-version ? "3.8" }:
pkgs:
_:
let
  pythonVersion = v: pkgs.lib.replaceStrings ["."] [""] v;
  inherit (pkgs) fetchFromGitHub;
in rec {
  pythonPackages = pkgs."python${pythonVersion python-version}".override {
    packageOverrides = final: current: {
      hypothesis = current.hypothesis.overridePythonAttrs(old: rec {
        version = "4.37.0";
        src = fetchFromGitHub {
          owner  = "HypothesisWorks";
          repo   = "hypothesis-python";
          rev    = "hypothesis-python-${version}";
          sha256 = "1j9abaapwnvms747rv8izspkgb5qa6jdkl7vp5ij9kfp1cvi7llp";
        };
        doCheck = false;
      });
    };
  };
}
