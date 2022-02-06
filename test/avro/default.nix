# This expression is meant to be built in CI.
#
# Check that Theta's command-line tool can generate Avro schemas
# without any errors.
{ pkgs }:

let
  inherit (pkgs) theta;
in

# Test that we can correctly handle Unicode text in Theta modules.
#
# Depending on Nix settings, it's possible to compile Haskell programs
# with their locale set to ASCII; this test should catch that problem
# for the main theta executable.
pkgs.runCommand "theta-avro-test" { src = ./.; } ''
  mkdir -p $out
  cd $out

  # avro type
  ${theta}/bin/theta --path $src/modules avro type -o unicode.avsc --type unicode.Foo

  # check unicode.avsc exists
  if [ ! -e unicode.avsc ]; then echo "unicode.avsc missing"; exit 1; fi

  # avro all
  ${theta}/bin/theta --path $src/modules avro all -m unicode -m example

  # Check avro/unicode/Foo.avsc exists
  if [ ! -e avro/unicode/Foo.avsc ]; then echo "avro/unicode/Foo.avsc missing"; exit 1; fi

  # Check avro/example/Bar.avsc exists
  if [ ! -e avro/unicode/Foo.avsc ]; then echo "avro/unicode/Foo.avsc missing"; exit 1; fi
''
