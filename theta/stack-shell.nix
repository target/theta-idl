{ ghc }:

# On any system with Nix, you can build theta with Stack using:
#
# > stack --nix --nix-shell-file stack-shell.nix build

let
  pkgs = import <nixpkgs> {};
in
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "theta";

  buildInputs = [ pkgs.zlib ];
}
