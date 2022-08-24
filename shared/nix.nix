{ pkgs
, system ? builtins.currentSystem
}:

let

get-flake =
  import
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "get-flake";
        rev = "703f15558daa56dfae19d1858bb3046afe68831a";
        sha256 = "1crp9fpvwg53ldkfxnh0wyxx470lm8bs025rck2bn5jn8jqmhj6f";
      });

purs-nix-base =
  get-flake
    (pkgs.fetchFromGitHub
      { owner = "ursi";
        repo = "purs-nix";
        rev = "18c1cae603b876c62515b7e4a3d4b587119e006b";
        sha256 = "0v78qgn0pdpmyy2wmyv0cig9mdflkcmaydgjqr6rxs4x3h1y4brv";
      }
    ) { inherit system; };

purescript-postgres = purs-nix-base.build
  { name = "purescript-postgres";
    info = {};
    src.path = pkgs.fetchFromGitHub
      { owner = "quelklef";
        repo = "purescript-postgres";
        rev = "cec8d32b614719bb21b7622dbd2d2fdb051fefcf";
        sha256 = "1a5pnv61fblm6xg4kfmlmbxbzni4hnd2pcl7kkd2jxzqgjhvan4f";
      };
  };

purs-nix =
  let
    original = purs-nix-base;
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
    [[ $(type -t ps1_push) == function ]] && ps1_push "y/${dir} "
  '';

in { inherit purs-nix mk-shellhook; }
