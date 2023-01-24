{ compiller ? null # Compiller name as string
, attr ? null # Attribute to place all the derivations into
}:

self: super:

let
  haskellOverlay = import ./haskellOverlay {
    haskellLib = super.haskell.lib;
    pkgs = self;
  };
  haskellPkgs = if compiller == null
  then self.haskellPackages
  else self.haskell.packages.${compiller};
  set = { inherit (haskellPkgs.extend haskellOverlay) burn-cli burn-gtk; };
in if attr == null
then set else { ${attr} = set; }
