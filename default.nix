{ pkgs ? import <nixpkgs> {}, emacs ? pkgs.emacs }:
let
  check-package = import (builtins.fetchGit {
    url = "https://github.com/akirak/emacs-package-checker";
    ref = "master";
    rev = "3f752d5dcc5d740446b36619cd34c97b6eb09225";
  });
in check-package {
  inherit emacs pkgs;
  name = "emacs-mozc-posframe";
  src = ./.;
  targetFiles = ["mozc-cand-posframe.el"];
  emacsPackages = epkgs: (with epkgs.melpaPackages; [
    posframe
    mozc
  ]);
}
