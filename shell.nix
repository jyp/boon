with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  ghc = haskell.compiler.ghc7103;
  name = "myEnv";
  buildInputs = [ ncurses zlib.dev zlib.out ];
  shellHook = ''
      export LANG=en_US.UTF-8
      '';
}