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
        numbers
        control
        arrays
        lazy
        node-fs
        stringutils
        random
        aff
        aff-promise
        postgres
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
    buildInputs = [
      (nixed.command {
        srcs = [ "$PWD/../client" "$PWD/../shared" ];
      })
      pkgs.python3
    ];

    shellHook = ''

      ${shared.mk-shellhook { dir = "client"; }}

      root=$PWD

      function y.client.serve {(
        cd $root
        purs-nix bundle &&
        python -m http.server
      )}

      function y.client.watch {(
        cd $root
        export root
        export -f y.client.serve
        git ls-files .. | xargs realpath | grep -E 'client|shared' | entr -csr y.client.serve
      )}

    '';
  };

}
