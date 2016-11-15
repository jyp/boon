with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      varwidth
                      lm
                      xargs
                      logreq
                      scheme-small wrapfig marvosym wasysym wasy cm-super unicode-math filehook lm-math capt-of
                      xstring ucharcat;
                  })
                ];
}
