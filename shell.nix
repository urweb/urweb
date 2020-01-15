let
  pkgs = import <nixpkgs> {};
  def = import ./default.nix;
in
pkgs.mkShell {
  buildInputs = def.buildInputs;
}
