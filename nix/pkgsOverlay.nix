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

  xrandr = "${self.xorg.xrandr}/bin/xrandr" ;
  awk = "${self.gawk}/bin/awk" ;

  display-flipper = super.writeTextFile {
    name = "display-flipper";
    text = ''
      flip=$1
      for x in $(${xrandr} --listmonitors | tail -n +2 | ${awk} '{print $4}') ; do
      case $flip in
      flip)
      ${xrandr} --output ''$x --reflect x ;;
      *)
      ${xrandr} --output ''$x --reflect normal ;;
      esac
      done
    '';
    executable = true;
    destination = "/bin/display-flipper.sh";
  };

  set = {
    inherit (haskellPkgs.extend haskellOverlay) burn-cli burn-gtk;
    inherit display-flipper ;
  };
in if attr == null
then set else { ${attr} = set; }
