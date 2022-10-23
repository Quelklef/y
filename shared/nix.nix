{ system }:

let

pkgs =
  let
    rev = "37cc765b36b0f74e4f18800e466496bacb366a35";
    src =
      builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  in
    import src {};

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
        rev = "8eb2221e763970d2c6ccf0102e2106de6533e2e4";
        sha256 = "sha256-uyiH6VDe2EAveX4n1WLuYJSNLwMAKzG64B3C/kGwjrY=";
      };
  };

purs-nix =
  let
    original = purs-nix-base;
    # add purescript-postgres to the package set
    patched = original // { ps-pkgs = original.ps-pkgs // { postgres = purescript-postgres; }; };

  in patched;


mk-shellhook = { dir }: ''
  # Personal thing
  [[ $(type -t ps1_push) == function ]] && ps1_push "y/${dir} "
'';

in { inherit pkgs purs-nix mk-shellhook; }
