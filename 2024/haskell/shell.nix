{mkShell, pkgs}:

mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc98
    cabal-install
    hpack
    hlint
    haskell.packages.ghc98.haskell-language-server

    zlib
  ];
}
