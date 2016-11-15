{ mkDerivation, base, containers, fetchgit, gasp, graphviz
, labeled-tree, lens, mtl, parsek, polynomials-bernstein, process
, reflection, stdenv, text, typography-geometry, vector
}:
mkDerivation {
  pname = "lp-diagrams";
  version = "2.1.0";
  src = fetchgit {
    url = "https://github.com/jyp/lp-diagrams.git";
    sha256 = "0n25cc2h863xgr119a0y8ip0pdazpljhjixlr2pvm9g18r23csw6";
    rev = "5a9196d14191f7d8d16a191676ad461e1fbce89f";
  };
  libraryHaskellDepends = [
    base containers gasp graphviz labeled-tree lens mtl parsek
    polynomials-bernstein process reflection text typography-geometry
    vector
  ];
  description = "An EDSL for diagrams based based on linear constraints";
  license = stdenv.lib.licenses.agpl3;
}
