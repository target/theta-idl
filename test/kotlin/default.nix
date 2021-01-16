# This expression is meant to be build in CI.  It doesn't produce any
# useful output, but if it builds properly, then the Kotlin code
# generation is passing tests in kotlin-tests/tests.ks
#
# If it fails, the Kotlin error message should printed with the call
# to nix build

{ pkgs ? import ../../nix/nixpkgs.nix {}
, theta ? import ../../theta { inherit pkgs; }
}:
let
  theta-generated-kotlin = pkgs.runCommand "theta-generated-kotlin" {
    THETA_LOAD_PATH = ./modules;
    THETA = "${theta}/bin/theta";
  } ''
    mkdir -p $out/theta/com/example
    $THETA kotlin --prefix com.example --module kotlin_tests --out $out/theta/com/example
  '';
in
pkgs.stdenv.mkDerivation {
  name = "theta-kotlin-tests";
  src = ./.;
  nativeBuildInputs = [ pkgs.kotlin theta ];
  installPhase = ''
      mkdir -p $out
      cp -r ${theta-generated-kotlin}/* $out

      kotlinc $out/theta -d $out/generated-kotlin.jar -cp $out/theta

      kotlinc $src/run-kotlin-tests.kt -include-runtime -d $out/run-kotlin-tests.jar -classpath $out/generated-kotlin.jar
      java -jar $out/run-kotlin-tests.jar
    '';
}
