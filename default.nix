{ pkgs ? import <nixpkgs> { } }:

pkgs.runCommand "y" {} ''
  mkdir $out
  cp -r ${import ./client} $out/client
  cp -r ${import ./server} $out/server
''
