#!/usr/bin/env nix-shell
with import <nixos-22.11> {};

let
  ghc = haskell.compiler.ghc925;

  hls = haskell-language-server.override {
    supportedGhcVersions = [ "92" ];
    };
  haskell-packages = haskell.packages.ghc925;
  ormolu = haskell-packages.ormolu;
  hpack = haskell-packages.hpack;
  apply-refact = haskell-packages.apply-refact;
in
haskell.lib.buildStackProject {
    name = "hls";
    buildInputs = [
      hls
      ghc
      stack
      hlint
      ormolu
      apply-refact
      openapi-generator-cli
    ];
    # shellHook doesn't work with zsh
    # SEE: https://github.com/chisui/zsh-nix-shell#shell-hooks
    # use local .zshrc with zsh plugin: https://github.com/freak2geek/zshrc
}
