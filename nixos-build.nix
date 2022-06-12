let pkgs = import ./default.nix { nixpkgs = <nixpkgs>; };
in pkgs.burn-gtk.components.exes.burn-gtk
