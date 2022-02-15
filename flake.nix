{
  description = "blog";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-unstable.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs.follows = "nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            ssg = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = { };
                haskell-language-server = { };
                hpack = { };
                dhall = { };
                stylish-haskell = { };
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.ssg.flake { };
      in flake // { defaultPackage = flake.packages."ssg:exe:ssg"; });
}
