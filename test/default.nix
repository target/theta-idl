{ pkgs ? import ../nix/nixpkgs.nix {}
, theta ? import ../theta { inherit werror; }
, werror ? false
}:

{
  avro   = import ./avro   { inherit pkgs theta; };
  hash   = import ./hash   { inherit pkgs theta; };
  kotlin = import ./kotlin { inherit pkgs theta; };
  python = import ./python { inherit pkgs theta; };
  rust   = import ./rust   { inherit pkgs theta; };
}
