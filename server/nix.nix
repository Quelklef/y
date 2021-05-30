{ pkgs ? import <nixpkgs> {} }:

let

inherit (import ../shared/nix.nix { inherit pkgs; }) purs-nix;

nixed = purs-nix.purs
  { src = builtins.filterSource (path: type: builtins.baseNameOf path != "client") ../.;
    dependencies =
      with purs-nix.ps-pkgs;
      let ns = purs-nix.ps-pkgs-ns; in
      [ console
        effect
        psci-support
        ordered-collections
        lists
        maybe
        node-fs
        stringutils
        refs
        argonaut-core
        argonaut-codecs
        argonaut-generic
        either
        foldable-traversable
        partial
        prelude
        strings
        bifunctors
        arrays
        node-buffer
        ns.ursi.debug
      ];
  };

in {

  deriv = pkgs.runCommand "y-server" {} ''
    mkdir $out
    cp -- ${nixed.modules.Main.bundle {}} $out/index.js
  '';

}
