{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "parsek";
  version = "1.0.4.0";
  src = fetchgit {
    url = "https://github.com/jyp/Parsek.git";
    sha256 = "0y13vymhjhhkss93khkxhfjqwzpkn81jzxb3v2ffgy0pjnmys9qc";
    rev = "79fe20addf8f4c4c5cdfc45164bf80b7634f3a71";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ];
  description = "Parallel Parsing Processes";
  license = stdenv.lib.licenses.gpl3;
}
