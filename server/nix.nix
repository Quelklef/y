{ system ? builtins.currentSystem }:

let

shared = import ../shared/nix.nix { inherit system; };

pkgs = shared.pkgs;

npmlock2nix =
  import
    (pkgs.fetchFromGitHub
      { owner = "tweag";
        repo = "npmlock2nix";
        rev = "5c4f247688fc91d665df65f71c81e0726621aaa8";
        sha256 = "1zkrcph1vqgl0q7yi9cg0ghq1mmvhy8rlc6xvyww56i3j87cg5gn";
      }
    ) { inherit pkgs; };

node_modules = npmlock2nix.node_modules { src = ./.; };

nixed = shared.purs-nix.purs
  { srcs = [ ../server ../shared ];

    dependencies =
      with shared.purs-nix.ps-pkgs;
      let ns = shared.purs-nix.ps-pkgs-ns; in
      [
        console
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
        aff
        aff-promise
        filterable
        postgres
        quickcheck
      ];

    foreign."Database.Postgres".node_modules = "${node_modules}/node_modules";
  };

purs-nix-bundle-args = {
  esbuild = {
    platform = "node";
    external =
      [ "pg-native" ]
        # ^ node library underlying purescript-postgres needs this
        #   seems to require() a dep it doesn't actually use ..?
      ++ [ "pg" "ws" ];
        # ^ uhhhh the nix-build needs these and idk why...
  };
};

local-postgres =
  import
    (pkgs.fetchFromGitHub
      { owner = "quelklef";
        repo = "local-postgres";
        rev = "7a313efd50eb7710fee02b719728028486ff4526";
        sha256 = "1qw9b97f1pp0fyji0z684b0ja8b32n24m19izqj7km45sczqgljx";
      }
    ) { inherit pkgs; };

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "y-server";
    src = ./.;

    buildInputs = [ pkgs.nodejs ];

    installPhase = ''
      mkdir $out

      cp ${nixed.modules.Main.bundle purs-nix-bundle-args} $out/index.js

      mkdir -p $out/node_modules/
      cp -r ${node_modules}/node_modules $out/

      echo "${pkgs.nodejs}/bin/node $out/index.js" > $out/run.sh
      chmod +x $out/run.sh
    '';
  };

  shell = pkgs.mkShell {
    buildInputs =
      [ (nixed.command {
          srcs = [ "$PWD/../server/src" "$PWD/../shared/src" ];
          test = "$PWD/test";
          test-module = "Y.Server.Test.Main";
          bundle = purs-nix-bundle-args;
        })
        pkgs.nodejs
        pkgs.entr
        local-postgres
      ];

    shellHook = ''

      ${shared.mk-shellhook { dir = "server"; }}

      root=$PWD

      function y.server.run {(
        cd $root
        [ -d ./pg ] || lpg make ./pg
        lpg do ./pg bash -c 'pg_ctl status || pg_ctl start' || return 1
        purs-nix bundle &&
        Y_DB_CONNECTION_STRING=$(lpg do ./pg bash -c 'echo $LPG_CONNSTR') \
          node --trace-uncaught ./main.js
      )}

      function y.server.watch {(
        cd $root
        export root
        export -f y.server.run
        git ls-files .. | xargs realpath | grep -E 'server|shared' | entr -csr y.server.run
      )}

    '';
  };

}
