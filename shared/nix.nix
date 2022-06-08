{ pkgs ? import <nixpkgs> { } }:

let

purs-nix =
  let
    original =
      import
        (builtins.fetchGit
           { url = "https://github.com/ursi/purs-nix.git";
             rev = "988505248316b1dc82504c8e25358da535e34bd6";
           }
        ) {};

    purescript-postgres = original.build
      { name = "purescript-postgres";
        repo = "https://github.com/Quelklef/purescript-postgres";
        rev = "bbf62bab3906c9ec8c6b93ac30c6309b780b2903";
      };

    # add purescript-postgres to the package set
    patched = original // { ps-pkgs = original.ps-pkgs // { postgres = purescript-postgres; }; };

  in patched;


mk-shellhook = { dir }: ''
    function y.workflow {
      if ! [[ "$(pwd)" == */y/${dir} ]]; then
        echo 1>&2 "Run in y/${dir}/"
        return 1
      fi

     ( cd .. && git ls-files | grep -E '^(${dir}|shared)' | entr -cs 'cd ${dir} && purs-nix bundle && echo done' )
    }

    # Personal thing
    [[ $(type -t ps1_scope) == function ]] && ps1_scope "[y/${dir}] "
  '';

in { inherit purs-nix mk-shellhook; }
