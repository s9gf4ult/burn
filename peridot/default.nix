{ mkDerivation, base, containers, dependent-map, dependent-sum
, dependent-sum-template, dlist, foldl, HUnit, profunctors, stdenv
, tasty, tasty-hunit, time, transformers, vector, vinyl
}:
mkDerivation {
  pname = "peridot";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers dependent-map dependent-sum dependent-sum-template
    dlist foldl profunctors time transformers vector vinyl
  ];
  testHaskellDepends = [
    base containers dependent-map dependent-sum dependent-sum-template
    dlist HUnit tasty tasty-hunit time vinyl
  ];
  homepage = "https://github.com/githubuser/peridot#readme";
  description = "Kinda framework for typesafe statistical reports";
  license = stdenv.lib.licenses.bsd3;
}
