# This expression is meant to be built in CI.
#
# Check that Theta's command-line tool correctly outputs type hashes.
{ pkgs }:
let
  inherit (pkgs) theta;

  # Note the tab characters in the expected strings!
  expected-1 = pkgs.writeTextFile {
    name = "theta-hash-expected-1";
    text = ''
      foo.Bar	4297466238902a227cd7992783897452
      foo.Baz	74aa1ef0f5de1b09a4eec622127f42d5
    '';
  };
  expected-2 = pkgs.writeTextFile {
    name = "theta-hash-expected-2";
    text = ''
      other.Other	06d2243dbfaeca8808608638d9c05d23
      other.OtherThing	20211f5221e58b43230122e99f51b6e4
      foo.Bar	4297466238902a227cd7992783897452
      foo.Baz	74aa1ef0f5de1b09a4eec622127f42d5
    '';
  };
  expected-enum = pkgs.writeTextFile {
    name = "theta-hash-expected-enum";
    text = ''
      enum.Foo	10ac7250eaaf54a514d6a7789ec45570
    '';
  };
in
pkgs.runCommand "theta-hash-test"
  { THETA_LOAD_PATH = ./modules; } ''
  mkdir -p $out

  ${theta}/bin/theta hash --type foo.Bar --type foo.Baz > $out/theta-hash-1
  diff ${expected-1} $out/theta-hash-1

  ${theta}/bin/theta hash -m importing_foo -m other > $out/theta-hash-2
  diff ${expected-2} $out/theta-hash-2

  ${theta}/bin/theta hash -m enum > $out/theta-hash-enum
  diff ${expected-enum} $out/theta-hash-enum
  ''
