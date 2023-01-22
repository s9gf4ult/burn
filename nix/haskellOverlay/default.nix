{ haskellLib }: self: super:

with haskellLib; {
  rndvid = super.callPackage ../../rndvid {} ;
  gtk3 = super.callPackage ./gtk3.nix ;
  cairo = super.callPackage ./cairo.nix ;
  gio = super.callPackage ./gio.nix ;
  glib = super.callPackage ./glib.nix ;
  pango = super.callPackage ./pango.nix ;
}
