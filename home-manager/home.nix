# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  nixGL,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.firefox

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./firefox.nix
  ] ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      input-fonts.acceptLicense = true;
      joypixels.acceptLicense = true;
    };
  };

  nixGL.packages = nixGL.packages;
  nixGL.defaultWrapper = "mesa";
  nixGL.installScripts = [ "mesa" ];

  home = {
    username = "k1";
    homeDirectory = "/home/k1";
  };

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    du-dust
    duf
    gping
    procs
    thefuck
    lsd
    sox
    pgcli

    rclone
    restic

    age
    colordiff

    (pkgs.wrapHelm pkgs.kubernetes-helm { plugins = [ 
      pkgs.kubernetes-helmPlugins.helm-secrets
      pkgs.kubernetes-helmPlugins.helm-diff 
    ];})
    helm-docs
    google-cloud-sdk
    awscli2
    terraform
    terraform-docs
    kubectl
    helmfile
    devbox
    krew
    kustomize
    argocd
    minio-client
    sops
    yq-go
    jq
  ];

  programs.nvf = {
    enable = true;
    # your settings need to go into the settings attribute set
    # most settings are documented in the appendix
    settings = {
      vim.viAlias = false;
      vim.vimAlias = true;
      vim.lsp = {
        enable = true;
      };    
      vim.autocomplete.nvim-cmp.enable = true;
      vim.languages.enableTreesitter = true;
    };
  };

  programs.chromium = {
    enable = true;
    package = (config.lib.nixGL.wrap pkgs.chromium);
  };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    "procs.toml".source = ~/.dotfiles/home/config/procs/config.toml;
    "procs.toml".target = ".config/procs/config.toml";

    # "htoprc".source = ~/.dotfiles/home/config/htop/htoprc;
    # "htoprc".target = ".config/htop/htoprc";

    "flake8rc".source = ~/.dotfiles/home/config/flake8/flake8rc;
    "flake8rc".target = ".config/flake8/flake8rc";

    "curlrc".source = ~/.dotfiles/home/curlrc;
    "curlrc".target = ".curlrc";

    "nanorc".source = ~/.dotfiles/home/nanorc;
    "nanorc".target = ".nanorc";

    "psqlrc".source = ~/.dotfiles/home/psqlrc;
    "psqlrc".target = ".psqlrc";

    "wgetrc".source = ~/.dotfiles/home/wgetrc;
    "wgetrc".target = ".wgetrc";

    "ssh_config".source = ~/.dotfiles/home/ssh/config;
    "ssh_config".target = ".ssh/config";
  };

  targets.genericLinux.enable = true;

  xdg = {
    enable = true;
    mime.enable = true;
  };
  # xdg.configFile."mimeapps.list".force = true;

  editorconfig.enable = true;
    editorconfig.settings = {
    "*" = {
      charset = "utf-8";
      end_of_line = "lf";
      trim_trailing_whitespace = true;
      insert_final_newline = true;
      max_line_width = 78;
      indent_style = "space";
      indent_size = 4;
    };
  };

  fonts.fontconfig.enable = true;

  # Enable home-manager and git
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "24.11"; # Please read the comment before changing.
}
