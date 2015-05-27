with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "ml4pg";
  src = ./.;
  buildInputs = [
    jre
    emacs
    emacs24Packages.proofgeneral
    graphviz
    coq
    ## FIXME: Weka is in Nix
  ];

  buildPhase = "";

  installPhase = ''
    mkdir -p $out/share
    cp -ra . $out/share/ml4pg
    mkdir -p $out/bin
    emacs_bin=${emacs}/bin/emacs
    cat << EOF > $out/bin/ml4pg
    #!/bin/sh
    ML4PG_HOME=$out/share/ml4pg $emacs_bin -l $out/share/ml4pg/ml4pg.el "$@"
    EOF
    chmod +x $out/bin/ml4pg
  '';

  shellHook = ''
    export ML4PG_HOME="$PWD/"
  '';
}
