{ pkgs ? import <nixpkgs> {}
, toProd ? false
, y-version
}:

let

uglify-js =
  let
    ujs-src = pkgs.fetchFromGitHub {
      owner = "mishoo";
      repo = "UglifyJS";
      rev = "70ceda5398535c7028682e05cc8b82009953e54d";
      sha256 = "09gnmmzwzn06lshnv5vp6hai2v8ngsjn3c76rf1d7c4nzlrn2w3p";
    };
  in
  pkgs.writeShellScript "uglifyjs" ''
    ${pkgs.nodejs}/bin/node ${ujs-src}/bin/uglifyjs "$@"
  '';

with-uglified = floc: deriv: pkgs.stdenv.mkDerivation {
  name = (deriv.name + "-uglified");
  src = deriv.outPath;
  installPhase = ''
    cp -r $src $out
    chmod +w $out/${floc}
    ${uglify-js} $src/${floc} -c toplevel -m -o $out/${floc}
  '';
};

y =
  if y-version == "use-local"
  then ./..
  else pkgs.fetchFromGitHub {
    owner = "quelklef";
    repo = "y";
    inherit (y-version) rev sha256;
  };

y-client = with-uglified "main.js" (import (y + /client/default.nix) { });
y-server = import (y + /server/default.nix) { };

y-client-mason =
  let
    src = pkgs.fetchFromGitHub {
      owner = "ursi";
      repo = "y-client";
      rev = "d1a1d17efbe125f0508961e19af32d322a47c6cf";
      sha256 = "sha256-sxQnnMwM6v2tHzD6Ku6v7wkysWXGms6CRga2dwc8sWY=";
    };
  in pkgs.stdenv.mkDerivation {
    name = "y-client-mason";
    inherit src;
    installPhase = ''
      mkdir $out
      cp $src/{index.html,main.js} $out
    '';
  };

host = if toProd then "165.227.67.28" else "165.22.46.18";
hostname = if toProd then "y.maynards.site" else "pre.y.maynards.site";
description = if toProd then "Y" else "Y staging server";

in

{
  network.description = description;
  network.enableRollback = true;

  y =
    { modulesPath, config, pkgs, ... }:
    {
      deployment.targetHost = host;

      imports = [ (modulesPath + "/virtualisation/digital-ocean-config.nix") ];

      networking.firewall.allowedTCPPorts = [
        22  # ssh
        80  # http
        8080  # http
        443  # https
        8081  # websocket
      ];

      services.nginx = {
        enable = true;

        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts.${hostname} = {
          default = true;
          forceSSL = true;
          enableACME = true;
          root = "${y-client}/";

          locations."= /version".extraConfig = ''
            return 200 '${if y-version == "use-local" then "working dir" else y-version.rev}';
            add_header Content-Type text/plain;
          '';

          locations."/mason/".extraConfig = ''
            alias ${y-client-mason}/;
            try_files $uri $uri/ =404;
          '';
        };
      };

      systemd.services.y = {
        description = "Y server";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        environment = { PORT = "8080"; };
        serviceConfig = {
          ExecStart = pkgs.writeShellScript "y-kickoff" ''
            set -euo pipefail
            export Y_SSL_CERT=$(cat /var/lib/acme/${hostname}/cert.pem)
            export Y_SSL_KEY=$(cat /var/lib/acme/${hostname}/key.pem)
            export Y_DB_CONNECTION_STRING='postgresql://y:y@localhost'
            ${pkgs.nodejs}/bin/node ${y-server}/index.js
          '';
          Type = "simple";
          Restart = "always";
        };
      };

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
        enableTCPIP = false;
        authentication = ''
          local all all trust
          host all all ::1/128 trust
        '';
        initialScript = pkgs.writeText "y-pg-init-script" ''
          CREATE USER y WITH PASSWORD 'y';
          CREATE DATABASE y;
          GRANT ALL PRIVILEGES ON DATABASE y TO y;
        '';
      };

      # for letsencrypt
      security.acme = {
        email = "eli.t.maynard+y-acme@gmail.com";
        acceptTerms = true;
      };
    };
}
