{ pkgs ? import <nixpkgs> { } }:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "f80db914129d14b021f51209709bb526b3734f3e";
       }
    ) {};

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
