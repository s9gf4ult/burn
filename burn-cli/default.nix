{ mkDerivation, base, bloodhound, burn, data-default, formatting
, http-client, lens, mtl, optparse-applicative, servant
, servant-client, servant-server, stdenv, stm, text, time, vector
, warp
}:
mkDerivation {
  pname = "burn-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bloodhound burn data-default formatting http-client lens mtl
    servant servant-client servant-server stm text time vector warp
  ];
  executableHaskellDepends = [ base burn optparse-applicative ];
  homepage = "https://github.com/githubuser/burn-cli#readme";
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
