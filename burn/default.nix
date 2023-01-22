{ mkDerivation, aeson, base, bytestring, cassava, containers
, data-default, dependent-map, dependent-sum
, dependent-sum-template, directory, dlist, foldl, http-client
, inflections, lens, lib, mtl, optparse-applicative, profunctors
, servant, servant-client, servant-server, statistics, stm, tasty
, tasty-hunit, text, time, transformers-base, vector, warp
}:
mkDerivation {
  pname = "burn";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cassava containers data-default dependent-map
    dependent-sum dependent-sum-template directory dlist foldl
    http-client inflections lens mtl optparse-applicative profunctors
    servant servant-client servant-server statistics stm text time
    transformers-base vector warp
  ];
  testHaskellDepends = [
    base data-default lens mtl tasty tasty-hunit time transformers-base
  ];
  homepage = "https://github.com/githubuser/burn#readme";
  description = "Simple project template from stack";
  license = lib.licenses.bsd3;
}
