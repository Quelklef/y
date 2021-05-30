{ pkgs ? import <nixpkgs> { } }:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "64843347061f6928e194cc1b1d1710ee5d556a19";
       }
    ) {};

in

{
  inherit purs-nix;
}
