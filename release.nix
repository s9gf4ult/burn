{ pkgs ? import <nixpkgs> {} }:

let
  compillerSet = pkgs.haskell.packages.ghc8107 ;
in

{ burn-cli = }
