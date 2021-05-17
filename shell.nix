{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (builtins.fetchGit {
      url = "https://github.com/justinwoo/easy-purescript-nix";
      rev = "fbbb27c1afd51d729939a6a2006e954dbd844846";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_14_0
    easy-ps.spago
  ];
}
