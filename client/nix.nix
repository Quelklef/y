{ pkgs ? import <nixpkgs> {} }:

let

inherit (import ../shared/nix.nix { inherit pkgs; }) purs-nix;

nixed = purs-nix.purs
  { srcs = [ ../client ../shared ];
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
    src = ./.;
    name = "y-client";

    installPhase = ''
      mkdir $out

      cp $src/index.html $out/
      cp ${nixed.modules.Main.bundle {}} $out/index.js

      echo "${pkgs.python3}/bin/python3.8 -m http.server -d \$(dirname \$(readlink -f \$0))" > $out/run.sh
      chmod +x $out/run.sh
    '';
  };

  shell = pkgs.mkShell {
    buildInputs =
      [ (nixed.command {
          srcs = [ "$PWD/../client" "$PWD/../shared" ];
        })
      ];
  };

}
