with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "ml4pg";
  src = ./.;
  buildInputs = [
    jre
    emacs
    graphviz
    coq
    emacs24Packages.proofgeneral
  ];
  shellHook = ''
    export CWD=$(pwd)
    export ML4PG_HOME="$CWD/"
  '';
}
