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
      gogol = haskellPackages.callPackage ./nix/gogol.nix {};
      gogol-core = haskellPackages.callPackage ./nix/gogol-core.nix {};
      gogol-bigquery = pkgs.haskell.lib.dontHaddock (haskellPackages.callPackage ./nix/gogol-bigquery.nix {});
    };
  };

  libRelease = haskellPackages.callCabal2nix "bq-lib" ./lib {};
  libDev = pkgs.haskell.lib.dontHaddock libRelease;

in {
  bq-lib = libRelease;
  shell = haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [libDev];
    buildInputs = with pkgs; [
      cabal-install
    ];
  };
}
