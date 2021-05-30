{ pkgs ? import <nixpkgs> {} }:

let

inherit (import ../shared/nix.nix { inherit pkgs; }) purs-nix;

nixed = purs-nix.purs
  { src = ../.;
    dependencies =
      with purs-nix.ps-pkgs;
      let ns = purs-nix.ps-pkgs-ns; in
      [ console
        effect
        psci-support
        ns.ursi.elmish
        ordered-collections
        lists
        maybe
        newtype
        refs
        argonaut-core
        argonaut-codecs
        argonaut-generic
        either
        foldable-traversable
        partial
        prelude
        strings
        transformers
        tuples
        ns.ursi.html
        bifunctors
        integers
        math
        control
        arrays
        lazy
        node-fs
        stringutils
      ];
  };

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "y-client";
    src = ../.;

    installPhase = ''
      mkdir $out
      cp -- ${nixed.modules.Main.bundle {}} $out/index.js
    '';
  };

}
