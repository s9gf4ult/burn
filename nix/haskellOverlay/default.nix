{ haskellLib, pkgs }: self: super:

let
  cabal2nix = "${pkgs.cabal2nix}/bin/cabal2nix" ;
  genDefault = path: pkgs.runCommand "dotnix" {} ''
    ${cabal2nix} ${path} > $out
  '' ;
in
with haskellLib; {
  burn = super.callPackage (genDefault ../../burn) {} ;
  burn-cli = super.callPackage (genDefault ../../burn-cli) {} ;
  burn-gtk = super.callPackage (genDefault ../../burn-gtk) {} ;

  gtk3 = super.callPackage ./gtk3.nix { inherit (pkgs) gtk3; };
  cairo = super.callPackage ./cairo.nix { inherit (pkgs) cairo; };
  gio = super.callPackage ./gio.nix { system-glib = pkgs.glib; };
  glib = super.callPackage ./glib.nix { inherit (pkgs) glib; };
  pango = super.callPackage ./pango.nix { inherit (pkgs) pango; };
}
