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
        rev = "66427405d2a3e0c2491646a6efc1716ce3731f3d";
        sha256 = "sha256-aArvsmkMc+LF2wEdLryiX/kqzMjtLsbcniIuSfFKgxg=";
      }
    ) { inherit system; };

purescript-postgres = let

  src =
    pkgs.fetchFromGitHub
      { owner = "quelklef";
        repo = "purescript-postgres";
        rev = "a4945f1636d1360f062f47fc3416f6833f3d9715";
        sha256 = "sha256-lZEtYpunWlaQ6uln5kTAq1IdWE3Xw5RFIEw7XOZyIKg=";
      };

  in purs-nix-base.build
    { name = "purescript-postgres";
      info = import "${src}/package.nix" { ps-pkgs = purs-nix.ps-pkgs; };
        # ^ hack to get around the fact that purs-nix doesn't properly handle
        #   the case when your package.nix imports another file
      src.path = src;
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
