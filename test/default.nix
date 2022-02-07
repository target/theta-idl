{ pkgs }:
pkgs.linkFarmFromDrvs "theta-tests" [
  (import ./avro   { inherit pkgs; })
  (import ./hash   { inherit pkgs; })
  (import ./kotlin { inherit pkgs; })
  (import ./python { inherit pkgs; })
  (import ./rust   { inherit pkgs; })
]
