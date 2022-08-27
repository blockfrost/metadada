{ config, pkgs, lib, ... }:
with lib;

let
  cfg = config.services.token-registry;

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./..;
  metadada = pkgs.haskellPackages.callCabal2nix "metadada" src {};

  dataDir = "/var/lib/token-registry";

  repoUrl =
    if cfg.testnet
    then "https://github.com/input-output-hk/metadata-registry-testnet"
    else "https://github.com/cardano-foundation/cardano-token-registry";
  subdir =
    if cfg.testnet
    then "registry"
    else "mappings";
in
{
  options = {
    services.token-registry = {
      enable = mkEnableOption "Enable local token registry";
      testnet = mkOption {
        type = types.bool;
        default = false;
        description = "Use testnet token registry";
      };
    };
  };

  config = mkIf cfg.enable {

    users.users.token-registry = {
      home = "${dataDir}";
      createHome = true;
      isSystemUser = true;
      group = "token-registry";
    };
    users.groups.token-registry = {};

    systemd.services.token-registry = {
      description = "Token registry updater";

      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.git ];
      script = ''
        set -e
        if [ -d repo ]; then
          git -C repo fetch
          old=$(git -C repo rev-parse @)
          new=$(git -C repo rev-parse @{u})
          if [ $old != $new ]; then
            git -C repo rebase
            echo "Updated from $old to $new"
            prevOut="$( readlink current )"
            nextOut="out.$( date +%s )"
            ${metadada}/bin/metadada-convert-dir repo/${subdir} $nextOut
            ln -sf $nextOut next
            mv -T next current
            rm -r $prevOut
          fi
        else
          git clone ${repoUrl} repo
          git -C repo config gc.autoDetach false
          echo "Initialized at $(git -C repo rev-parse @)"
          nextOut="out.$( date +%s )"
          ${metadada}/bin/metadada-convert-dir repo/${subdir} $nextOut
          ln -s $nextOut next
          mv -T next current
        fi
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "token-registry";
        WorkingDirectory = dataDir;
        Restart = "on-failure";
        RestartSec = "60";
      };
    };

    systemd.timers.token-registry = {
      wantedBy = [ "timers.target" ];
      timerConfig.OnUnitInactiveSec = "60m";
    };

     systemd.tmpfiles.rules = [
       "L+ /var/lib/blockfrost-token-registry - - - - ${dataDir}/current"
       "z ${dataDir} 755 token-registry token-registry - -"
     ];
  };
}
