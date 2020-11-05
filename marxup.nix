{ mkDerivation, base, configurator, containers, directory, dlist
, fetchgit, filepath, haskell-src-exts, labeled-tree, lens
, lp-diagrams, mtl, parsek, pretty, process, stdenv, text
}:
mkDerivation {
  pname = "marxup";
  version = "3.1.2.0";
  src = fetchgit {
    url = "https://github.com/jyp/MarXup.git";
    sha256 = "1pdg7i20ccq6gbqkx32ir1kyjhy4z0fa2i4lckv5fd10vkbyr9n1";
    rev = "67cca5362aa8a7adc49733971d57ae2496f556ba";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
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
