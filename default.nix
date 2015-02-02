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
  ];
  shellHook = ''
    export CWD=$(pwd)
    export ML4PG_HOME="$CWD/"
  '';
}
