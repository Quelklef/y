{ pkgs ? import <nixpkgs> { } }:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "f80db914129d14b021f51209709bb526b3734f3e";
       }
    ) {};

in { inherit purs-nix; }
