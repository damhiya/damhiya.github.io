{ pkgs ? import <nixpkgs> { } }:
let hpkgs = pkgs: with pkgs; [ pandoc shakespeare ];
in pkgs.mkShell { nativeBuildInputs = with pkgs; [ (ghc.withPackages hpkgs) ]; }
