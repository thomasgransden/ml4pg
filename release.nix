{nixpkgs, system}:

let pkgs = import nixpkgs { inherit system; };
in rec {
  # Run test suite
  ml4pg_test = import ./default.nix {
    release = false;
  };

  # Produce tarball
  ml4pg_release = import ./default.nix {
    release = true;
  };
}
