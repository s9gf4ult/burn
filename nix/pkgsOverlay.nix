self: super:
let haskellOverlay = import ./haskellOverlay {
  haskellLib = super.haskell.lib;
  pkgs = self;
};
in {
  inherit (self.haskellPackages.extend haskellOverlay) burn-cli burn-gtk ;
}
