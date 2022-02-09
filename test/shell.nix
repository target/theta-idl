{ pkgs }:
let
  cross-language = import ./cross-language {
    inherit pkgs;
    extra-build-tools = pkgs.rust-dev-tools;
  };
in
cross-language.env
