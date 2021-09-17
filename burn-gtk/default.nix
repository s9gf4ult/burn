{ mkDerivation, base, burn, containers, data-default, formatting
, gtk3, http-client, lens, lib, optparse-applicative, process
, servant, servant-client, stm, text, time, transformers-base
}:
mkDerivation {
  pname = "burn-gtk";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base burn containers data-default formatting gtk3 http-client lens
    optparse-applicative process servant servant-client stm text time
    transformers-base
  ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/githubuser/burn-gtk#readme";
  description = "Simple project template from stack";
  license = lib.licenses.bsd3;
}
