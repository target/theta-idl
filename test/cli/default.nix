# This expression is meant to be built in CI.
#
# Check that various Theta cli subcommands have expected output.
{ pkgs }:
let
  inherit (pkgs) theta;

  # Note the tab characters in the expected strings!
  expected-1 = pkgs.writeTextFile {
    name = "theta-hash-expected-1";
    text = ''
      foo.Bar	acbc45ad628ebccf04ec52d0d275a57c
      foo.Baz	261517de3a75f9f11bbdb3b496e08381
    '';
  };
  expected-2 = pkgs.writeTextFile {
    name = "theta-hash-expected-2";
    text = ''
      other.Other	a6fd63eefd826a232fb8bfc551200c1e
      other.OtherThing	814fc5791e3990d5f55d07e48f560889
      foo.Bar	acbc45ad628ebccf04ec52d0d275a57c
      foo.Baz	261517de3a75f9f11bbdb3b496e08381
    '';
  };
  expected-enum = pkgs.writeTextFile {
    name = "theta-hash-expected-enum";
    text = ''
      enum.Foo	c8cf29e0b679d8fd4878ff397292ca56
    '';
  };
  expected-list-names = pkgs.writeTextFile {
    name = "theta-list-names-expected";
    text = ''
      enum
      foo
      importing_foo
      nested.example
      other
    '';
  };
  expected-list-paths = pkgs.writeTextFile {
    name = "theta-list-paths-expected";
    text = ''
      modules/enum.theta
      modules/foo.theta
      modules/importing_foo.theta
      modules/nested/example.theta
      modules/other.theta
    '';
  };
  expected-list-both = pkgs.writeTextFile {
    name = "theta-list-both-expected";
    text = ''
      enum	modules/enum.theta
      foo	modules/foo.theta
      importing_foo	modules/importing_foo.theta
      nested.example	modules/nested/example.theta
      other	modules/other.theta
    '';
  };
in
pkgs.runCommandWith {
  name = "theta-cli-test";
  derivationArgs = {src = ./.;};
} ''
  cd $src
  export THETA_LOAD_PATH=modules

  mkdir -p $out

  ${theta}/bin/theta hash --type foo.Bar --type foo.Baz > $out/theta-hash-1
  diff ${expected-1} $out/theta-hash-1

  ${theta}/bin/theta hash -m importing_foo -m other > $out/theta-hash-2
  diff ${expected-2} $out/theta-hash-2

  ${theta}/bin/theta hash -m enum > $out/theta-hash-enum
  diff ${expected-enum} $out/theta-hash-enum

  ${theta}/bin/theta list modules > $out/theta-list-names
  diff ${expected-list-names} $out/theta-list-names

  ${theta}/bin/theta list modules --names > $out/theta-list-names
  diff ${expected-list-names} $out/theta-list-names

  ${theta}/bin/theta list modules --paths > $out/theta-list-paths
  diff ${expected-list-paths} $out/theta-list-paths

  ${theta}/bin/theta list modules --names --paths > $out/theta-list-both
  diff ${expected-list-both} $out/theta-list-both
  ''
