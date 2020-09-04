{ pkgs ? import <nixpkgs> {}, ... }:
with pkgs;
let
  ghcWithPackages = pkgs.ghc.withHoogle (hsPkgs: with hsPkgs; []);
in pkgs.mkShell {
  buildInputs = with pkgs; [
    ghcWithPackages
  ];
}
