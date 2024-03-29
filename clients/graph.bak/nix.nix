{ system ? builtins.currentSystem }:

let

shared = import ../../shared/nix.nix { inherit system; };

pkgs = shared.pkgs;

nixed = shared.purs-nix.purs
  { srcs = [ ./. ../../shared ];
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
        quickcheck
      ];
  };

purs-nix-bundle-args = {
  esbuild.format = "iife";
  # ^ necessary in some cases due to js bizareness
  #   compare 'var top = 5; console.log(top);'
  #   with '(function() { var top = 5; console.log(top); })'
};

in {

  deriv = pkgs.stdenv.mkDerivation {
    src = ./.;
    name = "y-client";

    installPhase = ''
      mkdir $out

      cp $src/index.html $out/
      cp ${nixed.modules.Main.bundle purs-nix-bundle-args} $out/main.js

      echo "${pkgs.python3}/bin/python3.8 -m http.server -d \$(dirname \$(readlink -f \$0))" > $out/run.sh
      chmod +x $out/run.sh
    '';
  };

  shell = pkgs.mkShell {
    buildInputs = [
      (nixed.command {
        srcs = [ "$PWD" "$PWD/../../shared" ];
        bundle = purs-nix-bundle-args;
      })
      pkgs.python3
      pkgs.entr
    ];

    shellHook = ''

      ${shared.mk-shellhook { dir = "clients/graph"; }}

      root=$PWD

      echo "run y.client.watch"

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
