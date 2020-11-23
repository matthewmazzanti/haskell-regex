{ pkgs ? import <nixpkgs> {}, ... }:
with pkgs;
let
  ghcWithPackages = pkgs.ghc.withHoogle (hsPkgs: with hsPkgs; [
    mtl
    containers
    QuickCheck
    criterion
  ]);
in pkgs.mkShell {
  buildInputs = with pkgs; [
    ghcWithPackages
  ];
}
