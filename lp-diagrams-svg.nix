{ mkDerivation, base, containers, fetchgit, FontyFruity, gasp
, JuicyPixels, lens, linear, lp-diagrams, lucid-svg, mtl
, optparse-applicative, stdenv, svg-tree, text, vector
}:
mkDerivation {
  pname = "lp-diagrams-svg";
  version = "1.1";
  src = fetchgit {
    url = "https://github.com/jyp/lp-diagrams-svg.git";
    sha256 = "139rz7qbwphssjbr4an6r6vfcdp9sz56g9vilx27v63a6v08qgp9";
    rev = "67cb2f31f7acc03d7982a7996c8301919489f9de";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers FontyFruity gasp JuicyPixels lens linear
    lp-diagrams lucid-svg mtl optparse-applicative svg-tree text vector
  ];
  description = "SVG Backend for lp-diagrams";
  license = "GPL";
}
