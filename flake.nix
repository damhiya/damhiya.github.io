{
  description = "blog";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        index-state = "2022-05-01T00:00:00Z";
        overlays = [
          haskellNix.overlay
          (final: prev: {
            ssg = final.haskell-nix.cabalProject' {
              inherit index-state;
              src = ./ssg;
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = {
                  inherit index-state;
                  version = "3.6.2.0";
                };
                haskell-language-server = {
                  inherit index-state;
                  version = "1.7.0.0";
                };
                hpack = {
                  inherit index-state;
                  version = "0.35.0";
                };
                dhall = {
                  inherit index-state;
                  version = "1.41.1";
                };
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.ssg.flake { };
      in flake // rec {
        packages.default = flake.packages."ssg:exe:ssg";
        apps.default = {
          type = "app";
          program = "${packages.default}/bin/ssg";
        };
      });
}
