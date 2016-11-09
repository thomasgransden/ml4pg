# Use this system's <nixpkgs> to fetch a known-good version (release 16.03)
with (let pkgs = import <nixpkgs> {};
          src  = pkgs.fetchFromGitHub {
            owner  = "NixOS";
            repo   = "nixpkgs";
            rev    = "d231868"; # 16.03 release
            sha256 = "0m2b5ignccc5i5cyydhgcgbyl8bqip4dz32gw0c6761pd4kgw56v";
          };
       in import "${src}" {});

stdenv.mkDerivation {
  name = "ml4pg";
  src = ./.;
  propagatedBuildInputs = [
    xdg_utils
    jre
    emacs
    emacs24Packages.proofgeneral
    graphviz
    coq
    ## FIXME: Weka is in Nix
  ];

  doCheck = true;

  checkPhase = ''
    ML4PG_HOME="$PWD/" ./test/runner.sh
  '';

  installPhase = ''
    mkdir -p $out/share
    cp -ra . $out/share/ml4pg
    mkdir -p $out/bin
    emacs_bin=${emacs}/bin/emacs
    cat << EOF > $out/bin/ml4pg
    #!/bin/sh
    if [ -z "$ML4PG_HOME" ]
    then
        export ML4PG_HOME=$out/share/ml4pg
    fi
    $emacs_bin -l $out/share/ml4pg/ml4pg.el "$@"
    EOF
    chmod +x $out/bin/ml4pg
  '';

  shellHook = ''
    export ML4PG_HOME="$PWD/"
  '';
}
