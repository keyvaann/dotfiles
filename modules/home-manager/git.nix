{ config, lib, pkgs, ... }: {
    home.packages = with pkgs; [
      mergiraf
    ];

    home.file.".config/git/gitattributes".text = ''
      *.java merge=mergiraf
      *.rs merge=mergiraf
      *.go merge=mergiraf
      *.js merge=mergiraf
      *.jsx merge=mergiraf
      *.json merge=mergiraf
      *.yml merge=mergiraf
      *.yaml merge=mergiraf
      *.toml merge=mergiraf
      *.html merge=mergiraf
      *.htm merge=mergiraf
      *.xhtml merge=mergiraf
      *.xml merge=mergiraf
      *.c merge=mergiraf
      *.cc merge=mergiraf
      *.h merge=mergiraf
      *.cpp merge=mergiraf
      *.hpp merge=mergiraf
      *.cs merge=mergiraf
      *.dart merge=mergiraf
      *.scala merge=mergiraf
      *.sbt merge=mergiraf
      *.ts merge=mergiraf
      *.py merge=mergiraf
    '';

    programs.git = {
      enable = true;

      userName = "Keyvan";
      userEmail = "keyvan42@pm.me";

      delta = {
        enable = true;
        options = {
          features = "hyperlinks";
          file-added-label = "[+]";
          file-copied-label = "[C]";
          file-decoration-style = "yellow ul";
          file-modified-label = "[M]";
          file-removed-label = "[-]";
          file-renamed-label = "[R]";
          file-style = "yellow bold";
          hunk-header-decoration-style = "omit";
          hunk-header-style = "syntax italic #303030";
          minus-emph-style = "syntax bold #780000";
          minus-style = "syntax #400000";
          plus-emph-style = "syntax bold #007800";
          plus-style = "syntax #004000";
          syntax-theme = "gruvbox-dark";
          width = 1;
        };
      };

      extraConfig = {
        advice.detachedHead = false;
        branch.autosetuprebase = "always";

        color = {
          branch = {
            current = "green reverse";
            local = "green";
            remote = "yellow";
          };

          status = {
            added = "green";
            changed = "yellow";
            untracked = "blue";
          };
        };

        core = {
          autocrlf = "input";
          untrackedCache = true;
          attributesFile = "~/.config/git/gitattributes";
        };

        rerere = {
            enabled = 1;
            autoupdate = 1;
        };

        alias = {
            # Sometimes, you might have done something wrong in git. You think you've lost your commits, or something like that.
            # Chances are, the information is still there--so the best course of action is to make an immediate backup, before you risk actually losing data.            
            panic = "!tar cvf ../git_panic.tar *";
        };

        diff = {
          colorMoved = "default";
          submodule = "log";
        };

        init.defaultBranch = "main";

        merge = {
            # Include summaries of merged commits in newly created merge commit messages
            log = "true";
        };
        merge.mergiraf = {
          name = "mergiraf";
          driver = "${lib.getExe pkgs.mergiraf} merge --git %O %A %B -s %S -x %X -y %Y -p %P";
        };

        push.default = "current";

        diff.sopsdiffer = {
            textconv = "sops -d";
        };

        filter.lfs = {
            clean = "git-lfs clean -- %f";
            smudge = "git-lfs smudge -- %f";
            process = "git-lfs filter-process";
            required = "true";
        };

        rebase = {
          autostash = true;
          autosquash = true;
        };

        user.useConfigOnly = true;
      };

      ignores = [];
    };
}