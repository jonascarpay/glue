let
  hnix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
in
  { pkgs ? import <nixpkgs> hnix }:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  }
