{ pkgs ? import <nixpkgs> {} }:

let

shared = import ../shared/nix.nix { inherit pkgs; };

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
        aff
        aff-promise
        filterable
        postgres
      ];

    foreign."Database.Postgres".node_modules = "${node_modules}/node_modules";
  };

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "y-server";
    src = ./.;

    buildInputs = [ pkgs.nodejs ];

    installPhase = ''
      mkdir $out

      cp ${nixed.modules.Main.bundle {}} $out/index.js

      mkdir -p $out/node_modules/
      cp -r ${node_modules}/node_modules $out/

      echo "${pkgs.nodejs}/bin/node $out/index.js" > $out/run.sh
      chmod +x $out/run.sh
    '';
  };

  shell = pkgs.mkShell {
    buildInputs =
      [ (nixed.command {
          srcs = [ "$PWD/../server" "$PWD/../shared" ];
          bundle.esbuild = {
            platform = "node";
            external = [ "pg-native" ];
              # ^ node library underlying purescript-postgres needs this
              #   seems to require() a dep it doesn't actually use ..?
          };
        })
        pkgs.nodejs
        pkgs.postgresql
      ];

    shellHook = ''
      ${shared.mk-shellhook { dir = "server"; }}

      root=$PWD

      function y.run-server {
        local args="$@"
        (
          cd "$root" && echo index.js \
            | ${pkgs.entr}/bin/entr -c \
            ${pkgs.nodejs}/bin/node --trace-uncaught index.js "$@"
        )
      }

      function y.pg-init {
        mkdir -p "$root"/pg/{cluster,socket}
        initdb "$root"/pg/cluster
        y.pg-start
        createdb y
        createuser y
      }
      export PGDATA=$PWD/pg/cluster

      function y.pg-start {
        pg_ctl -l "$root"/pg/log -o "--unix_socket_directories='$root/pg/socket'" start
        # stop with pg_ctl stop
      }
      export PGHOST=$root/pg/socket

      function y.pg-obliterate {
        # Sometimes useful
        ps -aux | grep postgres | awk '{ print $2 }' | xargs sudo kill -9
      }

      export LC_ALL=C.UTF-8  # fix postgres
      export Y_DB_CONNECTION_STRING="postgresql://y@localhost?host=$root/pg/socket"
    '';
  };

}
