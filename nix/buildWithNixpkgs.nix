{ nixpkgs ? import <nixpkgs>
}:

let
  pkgsOverlay = import ./pkgsOverlay.nix {} ;
  pkgs = nixpkgs { overlays = [pkgsOverlay ]; };
in [ pkgs.burn-cli pkgs.burn-gtk ]
