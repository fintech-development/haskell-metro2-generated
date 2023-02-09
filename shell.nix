#!/usr/bin/env nix-shell
with import <nixpkgs> {};

let
  ghc = haskell.compiler.ghc8107;
  #hls = haskell-language-server; # .override { supportedGhcVersions = [ "8107" ]; };
  ormolu = haskell.packages.ghc8107.ormolu;
  apply-refact = haskell.packages.ghc8107.apply-refact;
in
haskell.lib.buildStackProject {
    name = "hls";
    buildInputs = [
      #hls
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
