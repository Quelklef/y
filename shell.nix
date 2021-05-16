{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs =[
    pkgs.purescript
    pkgs.spago
  ];
}
