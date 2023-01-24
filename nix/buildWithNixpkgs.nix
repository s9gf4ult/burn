{ nixpkgs ? import <nixpkgs>
}:

let
  pkgsOverlay = import ./pkgsOverlay.nix { attr = "burn"; } ;
  pkgs = nixpkgs { overlays = [pkgsOverlay ]; };
in pkgs.burn
