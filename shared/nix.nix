{ pkgs ? import <nixpkgs> { } }:

let

purs-nix =
  import
    (builtins.fetchGit
       { url = "https://github.com/ursi/purs-nix.git";
         rev = "4b68c019d14e7f1e48ded85224e3e7a5a985cf1e";
       }
    ) {};

mk-shellhook = { dir }: ''
    function y-workflow {
      if ! [[ "$(pwd)" == */y/${dir} ]]; then
        echo 1>&2 "Run in y/${dir}/"
        return 1
      fi

      cd ..
      git ls-files | grep -E '^(${dir}|shared)' | entr -cs 'cd ${dir} && purs-nix bundle && echo done'
    }

    # Personal thing
    [[ $(type -t ps1_scope) == function ]] && ps1_scope "[y/${dir}] "
  '';

in { inherit purs-nix mk-shellhook; }
