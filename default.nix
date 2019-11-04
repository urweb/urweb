let
  pinnedNixpkgs = import (builtins.fetchTarball {
    name = "pinned-nixpkgs-for-urweb-school";
    url = https://github.com/NixOS/nixpkgs/archive/5a8bfc98a23669f71596d079df20730ccdfdf04b.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "15qbfjjw5ak1bpiq36s0y9iq3j45azmb8nz06fpx4dgkg32i8fm5";
  }) {};
in
{pkgs ? pinnedNixpkgs}: pkgs.callPackage ./derivation.nix {}
