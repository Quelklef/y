{ pkgs ? import <nixpkgs> {} }:
(import ./nix.nix { inherit pkgs; }).deriv
