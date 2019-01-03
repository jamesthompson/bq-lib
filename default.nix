{ compiler ? "ghc843", bootstrap ? import <nixpkgs> {} }:

let

  nixpkgs = import (bootstrap.fetchgit {
    url = "https://github.com/nixos/nixpkgs.git";
    rev = "6a3f5bcb061e1822f50e299f5616a0731636e4e7";
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
    fetchSubmodules = true;
  }) { config = { allowUnfree = true; }; };

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
    };
  };

  libRelease = haskellPackages.callCabal2nix "big-query-lib" ./lib {};
  libDev = pkgs.haskell.lib.dontHaddock libRelease;

in {
  big-query-lib = libRelease;
  shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [libDev];
    buildInputs = with pkgs; [
      cabal-install
    ];
  };
}
