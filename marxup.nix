{ mkDerivation, base, configurator, containers, directory, dlist
, fetchgit, filepath, haskell-src-exts, labeled-tree, lens
, lp-diagrams, mtl, parsek, pretty, process, stdenv, text
}:
mkDerivation {
  pname = "marxup";
  version = "3.1.0.0";
  src = fetchgit {
    url = "https://github.com/jyp/marxup.git";
    sha256 = "1flpdaxxiqacg1m8ac76a32qv78yp1721nlgnnw2kvpqyqhjs8za";
    rev = "ac35153d20e6f6d628b5a489a7fddd1ef0cb1140";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory filepath haskell-src-exts labeled-tree
    lens lp-diagrams mtl process text
  ];
  executableHaskellDepends = [
    base configurator dlist parsek pretty
  ];
  description = "Markup language preprocessor for Haskell";
  license = stdenv.lib.licenses.gpl2;
}
