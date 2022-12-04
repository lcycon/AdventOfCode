{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs {}}:

pkgs.mkShell {
  buildInputs = [ pkgs.stack pkgs.z3 pkgs.ghcid pkgs.haskell-language-server ];
}
