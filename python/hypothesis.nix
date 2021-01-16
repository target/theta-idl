{ pkgs ? import ../nix/nixpkgs.nix {}
, python ? pkgs.python37
}:
python.pkgs.hypothesis.overridePythonAttrs(old: rec {
  version = "4.37.0";
  src = pkgs.fetchFromGitHub {
    owner  = "HypothesisWorks";
    repo   = "hypothesis-python";
    rev    = "hypothesis-python-${version}";
    sha256 = "1j9abaapwnvms747rv8izspkgb5qa6jdkl7vp5ij9kfp1cvi7llp";
  };
  doCheck = false;
})
