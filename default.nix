let
  pinnedNixpkgs = import (builtins.fetchTarball {
      name = "pinned-nixpkgs-for-urweb-school";
      url = https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz;
      # Hash obtained using `nix-prefetch-url --unpack <url>`
      sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
    }) {};
in
{pkgs ? pinnedNixpkgs}: pkgs.callPackage ./derivation.nix {}
