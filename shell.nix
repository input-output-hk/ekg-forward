# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, minimal ? false
, withHoogle ? (! minimal)
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = ekgForwardHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      cabal-install
      stylish-haskell
      nix
      niv
      pkgconfig
    ] ++ lib.optionals (! minimal) [
      ghcid
      pkgs.git
      hlint
      stylish-haskell
      weeder
    ];

    inherit withHoogle;
  };

in

 shell 
