#!/usr/bin/env nix-shell
with import <nixpkgs-unstable> {};

let
  ghc = haskell.compiler.ghc927;

  hls = haskell-language-server.override {
    supportedGhcVersions = [ "927" ];
    };
  haskell-packages = haskell.packages.ghc927;
  ormolu = haskell-packages.ormolu;
  hpack = haskell-packages.hpack;
  apply-refact = haskell-packages.apply-refact;
in
haskell.lib.buildStackProject {
    name = "hls";
    buildInputs = [
      glpk
      pcre
      zlib
      xz
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
