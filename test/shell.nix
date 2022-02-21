{ pkgs, lib }:
let
  cross-language = import ./cross-language {
    inherit pkgs lib;
    extra-build-tools = pkgs.rust-dev-tools;
  };
in
cross-language.env
