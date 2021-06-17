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
      ];
  };

  # We have 1 node dependency
  # I don't want to deal with node2nix or anything, so we'll handle it manually
  node_ws = builtins.fetchTarball {
    url = "https://registry.npmjs.org/ws/-/ws-7.4.6.tgz";
    sha256 = "1xny147xgyqyghxcxgdvcm82fwkawlxc2qgcnvldcw4mcrdqdjqz";
  };

in {

  deriv = pkgs.stdenv.mkDerivation {
    name = "y-server";
    src = ./.;

    buildInputs = [ pkgs.nodejs ];

    installPhase = ''
      mkdir $out

      cp ${nixed.modules.Main.bundle {}} $out/index.js

      mkdir -p $out/node_modules/ws
      cp -r ${node_ws}/* $out/node_modules/ws

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
      ];

    shellHook = ''
      ${shared.mk-shellhook { dir = "server"; }}

      function y-run-server {
        if ! [[ "$(pwd)" == *y/server ]]; then
          echo >&2 "Run in y/server/"
        fi

        # [ -e ./server.key -a -e ./server.key.pub ] || ssh-keygen -t rsa -N "" -f ./server.key

        echo index.js | ${pkgs.entr}/bin/entr -c ${pkgs.nodejs}/bin/node index.js
      }
    '';
  };

}
