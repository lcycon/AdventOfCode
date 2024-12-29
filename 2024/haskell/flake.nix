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
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.ghc98
            cabal-install
            hpack
            hlint
            haskell.packages.ghc98.haskell-language-server

            zlib
          ];
        };
      });
}
