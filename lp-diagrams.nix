{ mkDerivation, base, containers, fetchgit, gasp, graphviz
, labeled-tree, lens, mtl, parsek, polynomials-bernstein, process
, reflection, stdenv, text, typography-geometry, vector
}:
mkDerivation {
  pname = "lp-diagrams";
  version = "2.1.4";
  src = fetchgit {
    url = "https://github.com/jyp/lp-diagrams.git";
    sha256 = "1c6zr5w2bbc35x8ncar8dwv2awfgkvxzql5g8r79vzyaqglnkayj";
    rev = "3b25b411d246d2b139377094f25128dacab67310";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers gasp graphviz labeled-tree lens mtl parsek
    polynomials-bernstein process reflection text typography-geometry
    vector
  ];
  description = "An EDSL for diagrams based based on linear constraints";
  license = stdenv.lib.licenses.agpl3;
}
