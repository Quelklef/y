{ inputs =
    { make-shell.url = "github:ursi/nix-make-shell/1";
      nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      ps-tools.follows = "purs-nix/ps-tools";
      purs-nix.url = "github:ursi/purs-nix/ps-0.15";
      utils.url = "github:ursi/flake-utils/8";
    };

  outputs = { utils, ... }@inputs:
    utils.apply-systems { inherit inputs; }
      ({ make-shell, pkgs, ps-tools, purs-nix, ... }:
         let

           local-postgres = let
             src = pkgs.fetchFromGitHub {
                 owner = "quelklef";
                 repo = "local-postgres";
                 rev = "d3f377454632b10621af009313b3700424b369cf";
                 sha256 = "sha256-67zbZjimGxYdx8rRpd0PBlMc7LMTUUNobDUpadfSlsA=";
             };
             in import src { inherit pkgs; };

           inherit (purs-nix) build ps-pkgs ps-pkgs-ns purs;

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

           y-repo =
             { repo = "https://github.com/Quelklef/y.git";
               rev = "4422547a0a484737b4f3e97410c4c893d2d130fc";
             };

           y = fetchGit { url = y-repo.repo; inherit (y-repo) rev; };

           postgres-pkg = build {
              name = "postgres";

              src.git =
                { repo = "https://github.com/Quelklef/purescript-postgres.git";
                  rev = "cec8d32b614719bb21b7622dbd2d2fdb051fefcf";
                };

              info.dependencies =
                with ps-pkgs;
                [
                  aff
                  aff-promise
                  effect
                  lists
                  arrays
                  maybe
                  either
                  argonaut-core
                  argonaut-codecs
                  argonaut-generic
               ];
            };

           y-shared-pkg = build {
             name = "y-shared";
             src.git = { inherit (y-repo) repo rev; };
             info =
               { src = "shared/src";
                 dependencies =
                   with ps-pkgs;
                   [ argonaut-generic
                     console
                     postgres-pkg
                     quickcheck
                   ];
               };
           };

           websocket-pkg = build {
             name = "websocket";
             src.git = { inherit (y-repo) repo rev; };
             info =
               { src = "shared/src";
                 install = "mkdir $out; cp client/src/WebSocket.* $out";
               };
           };

           inherit
             (purs
                { dependencies =
                    with ps-pkgs;
                    let inherit (ps-pkgs-ns) ursi; in
                    [ stringutils
                      ursi.elmish
                      ursi.prelude
                      y-shared-pkg
                      websocket-pkg
                      mation-pkg

                      # fake deps to hack around purs-nix only
                      # accepting a `build`'t package if it's top-level
                      postgres-pkg
                    ];
                }
             )
             command;
         in
         { devShell = let

               # line-endings on live-server are wrong; see <https://github.com/tapio/live-server/issues/418>
               # FIXME NONCRUCIAL: more recent nixpkgs have non-broken live-server
               live-server-fixed = pkgs.stdenv.mkDerivation {
                 name = "live-server-fixed";
                 buildInputs = [ pkgs.dos2unix ];
                 src = pkgs.nodePackages.live-server;
                 installPhase = ''
                   cp -r $src/. ./src
                   chmod +w -R ./src
                   dos2unix --verbose ./src/lib/node_modules/live-server/live-server.js
                   mkdir -p $out
                   cp -r ./src/. $out
                 '';
               };

             in make-shell
               { packages =
                   with pkgs;
                   [ entr
                     nodejs
                     live-server-fixed
                     purs-nix.purescript
                     ps-tools.for-0_15.purescript-language-server
                     (command {})
                     local-postgres
                   ];

                 aliases.watch = "find src | entr -s 'echo bundling; purs-nix bundle'";

                 functions.run-servers =
                   ''
                   trap : SIGINT
                   [ -e ./pg ] || lpg make pg
                   lpg pg-up ./pg
                   source <(lpg env ./pg)
                   export Y_DB_CONNECTION_STRING=$LPG_CONNSTR
                   $(nix-build ${y}/server --no-out-link)/run.sh &
                   purs-nix bundle
                   live-server --watch=./main.js --no-browser
                   kill %
                   '';
               };
         }
      );
}
