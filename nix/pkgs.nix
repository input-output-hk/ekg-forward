{ sources }:
# our packages overlay
pkgs: _: with pkgs; {
  ekgForwardHaskellPackages = import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
    inherit (sources) CHaP;
  };
}
