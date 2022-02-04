{ sources ? import ../nix/sources.nix
, pkgs ? import ../nix/nixpkgs.nix { inherit sources; }
, compiler-version ? "ghc884"
}:
let
  theta = import ./default.nix { inherit sources pkgs compiler-version; };

  tools = with pkgs.haskellPackages; [
    cabal-install
    stylish-haskell
    pkgs.time-ghc-modules
  ];
in
pkgs.lib.overrideDerivation theta.env (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ tools;

  # Workaround for Lorri; see:
  # https://github.com/target/lorri/issues/383
  shellHook = ''
    unset SOURCE_DATE_EPOCH
  '';
})
