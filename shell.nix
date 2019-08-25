with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz) {};
let hp = haskellPackages.override{
      overrides = self: super: {
        lp-diagrams = self.callPackage ./lp-diagrams.nix {};
        marxup = self.callPackage ./marxup.nix {};
      };};  
    ghc = hp.ghcWithPackages (ps: with ps; ([ base gasp lens lp-diagrams lp-diagrams-svg ]));
    myTexLive =  texlive.combine {
      inherit (texlive)
      make4ht # html conversion 
      tex4ht  # html conversion
      hyphenat # don't hyphenate
      dvipng # org-mode preview needs this
      arabtex
      ebgaramond # font
      #fonts
      tex-gyre
      tex-gyre-math
      collection-fontsrecommended
      comment
      dejavu
      doublestroke # usepackage dsfont
      inconsolata
      latexmk
      libertine
      libertinus
      # collection-fontutils
      biblatex
      capt-of
      cm-super
      # dvipng # org-mode preview wants this; but broken
      mathdesign # jlm requirement
      enumitem
      fancyhdr
      filehook
      hyperref
      lastpage
      lazylist # for lhs2tex
      lm
      lm-math
      logreq
      marvosym
      multirow
      newunicodechar
      newtx # newtxmath
      pgfplots
      polytable # for lhs2tex
      scheme-small
      soul
      stmaryrd
      titlesec # change the environment of sections, etc. in convenient way
      titling # tweaking title
      fontaxes
      threeparttable
      todonotes
      ucharcat
      unicode-math
      varwidth
      wasy
      wasysym
      wrapfig
      xargs
      xstring;
    };
in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ z3 myTexLive ];
  shellHook = ''
 export LANG=en_US.UTF-8
'';
}

