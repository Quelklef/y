{ pkgs ? import <nixpkgs> {} }:

let

shared = import ../shared/nix.nix { inherit pkgs; };

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
      ];
  };

npmlock2nix =
  let fetched = pkgs.fetchFromGitHub {
        owner = "tweag";
        repo = "npmlock2nix";
        rev = "8ada8945e05b215f3fffbd10111f266ea70bb502";
        sha256 = "0ni3z64wf1cha7xf5vqzqfqs73qc938zvnnbn147li1m4v8pnzzx";
      };
  in import fetched { inherit pkgs; };

node_modules = npmlock2nix.node_modules { src = ./.; };

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
        })
        pkgs.nodejs
        pkgs.postgresql
      ];

    shellHook = ''
      ${shared.mk-shellhook { dir = "server"; }}

      root=$PWD

      function y.run-server {(
        cd "$root" && echo index.js | ${pkgs.entr}/bin/entr -c ${pkgs.nodejs}/bin/node --trace-uncaught index.js
      )}

      function y.pg-init {
        mkdir -p "$root"/pg/{cluster,socket}
        initdb "$root"/pg/cluster
        pg-start
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
