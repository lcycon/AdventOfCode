{
  description = "idscanner";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in {
        devShells = {
          haskell = pkgs.callPackage ./haskell/shell.nix {};
          prolog = pkgs.callPackage ./prolog/shell.nix {};
          rust = pkgs.callPackage ./rust/shell.nix {};
        };
      });
}
