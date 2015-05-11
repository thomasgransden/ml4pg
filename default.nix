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
  shellHook = ''
    export CWD=$(pwd)
    export ML4PG_HOME="$CWD/"
  '';
}
