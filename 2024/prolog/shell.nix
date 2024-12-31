{mkShell, pkgs}:

mkShell {
  buildInputs = with pkgs; [
    swi-prolog
  ];
}
