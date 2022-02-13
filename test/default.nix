{ pkgs, lib }:
pkgs.linkFarmFromDrvs "theta-tests" [
  (import ./avro           { inherit pkgs; })
  (import ./hash           { inherit pkgs; })
  (import ./kotlin         { inherit pkgs; })
  (import ./python         { inherit pkgs lib; })
  (import ./rust           { inherit pkgs; })
  (import ./cross-language { inherit pkgs lib; })
]
