{ mkDerivation, base, gasp, lens, lp-diagrams, marxup, stdenv }:
mkDerivation {
  pname = "boon";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gasp lens lp-diagrams marxup ];
  description = "A generator of nix files";
  license = "GPL";
}
