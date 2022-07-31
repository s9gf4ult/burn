{ pkgs ? import <nixpkgs> {} }:
let
  compillerSet = pkgs.haskell.packages.ghc8107 ;
in

compillerSet.shellFor {
  packages = p: with p; [ gtk3 ] ;
  buildInputs = with pkgs; [pkgconfig zlib cairo glib pango gtk3 ] ;
}


# pkgs.mkShell {
# }
