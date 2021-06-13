{ pkgs ? import <nixpkgs> {} }:

let

shared = import ../shared/nix.nix { inherit pkgs; };

nixed = shared.purs-nix.purs
  { srcs = [ ../client ../shared ];
    dependencies =
      with shared.purs-nix.ps-pkgs;
      let ns = shared.purs-nix.ps-pkgs-ns; in
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
        random
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

    shellHook = ''
      ${shared.mk-shellhook { dir = "client"; }}

      function y-serve-client {
        if ! [[ "$(pwd)" == */y/client ]]; then
          echo >&2 "Run in y/client/"
          return 1
        fi

        ${pkgs.python3}/bin/python -m http.server
      }
    '';
  };

}
