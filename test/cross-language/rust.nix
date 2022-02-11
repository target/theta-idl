{ pkgs, lib }:
lib.theta-rust {
  name = "test-modules";
  src = ./.;
  theta-paths = [ ./modules ];
  modules = ["everything" "primitives"];
}
