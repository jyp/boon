{ mkDerivation, base, binary, containers, fetchgit, mtl, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "gasp";
  version = "1.3.0.0";
  src = fetchgit {
    url = "https://github.com/jyp/gasp.git";
    sha256 = "0fd18x83sjxnqkbikb93rdl2vffmxh3835isiy1b7ilikbdpkmx5";
    rev = "c70466868c8436a759f0603815a22d33a4fe38cf";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base binary containers mtl QuickCheck ];
  description = "A framework of algebraic classes";
  license = stdenv.lib.licenses.bsd3;
}
