{ pkgs ? import <nixpkgs> {} }:

let
  compillerSet = pkgs.haskell.packages.ghc8107 ;
  p = compillerSet.override {
    overrides = self: super: {
      burn = self.callPackage ./burn {} ;
      burn-cli = self.callPackage ./burn-cli {} ;
      burn-gtk = self.callPackage ./burn-gtk {} ;
    } ;
  } ;
in

{ inherit (p) burn-cli burn-gtk ; }
