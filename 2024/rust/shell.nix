{ mkShell, pkgs }:

mkShell { buildInputs = with pkgs; [ rustup z3 ]; }
