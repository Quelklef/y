{ system ? builtins.currentSystem }:

let

shared = import ../../shared/nix.nix { inherit system; };
inherit (shared) pkgs purescript-postgres-pkg get-flake;

purs-nix =
  get-flake
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "purs-nix";
        rev = "8729b7fbb02822df3fb3988bae94b276e50ca6fc";
        sha256 = "09bkggxiq7mc6yimg33g8mmncxs9xns7agz99ahxy5w3y3l63hkf";
      }
    ) { inherit system; };

mation-pkg =
  purs-nix.build {
    name = "mation";
    info = /package.nix;
    src.path =
      pkgs.fetchFromGitHub
        { owner = "quelklef";
          repo = "mation";
          rev = "425bba4ac9939aee2998518063e90a8a6e23271d";
          sha256 = "1s5kqf2rs8ll8bql65hvddk2fgrfpamb8kczcd22mrxkizmz9rln";
        };
  };

nixed = purs-nix.purs
  { srcs = [ ./. ../../shared ];
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
        numbers
        control
        arrays
        lazy
        node-fs
        stringutils
        random
        aff
        aff-promise
        purescript-postgres-pkg
        quickcheck
        mation-pkg
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
