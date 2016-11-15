{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }: 
with (import <nixpkgs> {}).pkgs;
let hp = haskell.packages.${compiler}.override{
    overrides = self: super: {
      lp-diagrams = self.callPackage ./lp-diagrams.nix {};
      marxup = self.callPackage ./marxup.nix {};
      };};
    locpkg = hp.callPackage ./default.nix { }; 
in stdenv.mkDerivation {
    name = locpkg.name;
    buildInputs = locpkg.buildInputs ++ [ z3 ];
    shellHook = ''
      export LANG=en_US.UTF-8
      '';
     }
