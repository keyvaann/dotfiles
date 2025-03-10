{ config, lib, pkgs, inputs, nixGL, ...}:
{
    home.packages = with pkgs; [
      # rofi-wayland-unwrapped
      # rofimoji
      swappy # snapshot editing tool
      kanshi
      grim
      slurp
      dunst
      waybar
      swaykbdd
      ffmpegthumbnailer
      blueman
      clipman
      gammastep
      playerctl
      brightnessctl
      pavucontrol # pulseaudio volume controle (GUI)
      swayosd

      xdotool # for rofi-emoji to insert emojis directly

      jellyfin-media-player
      gimp
      libreoffice
    ];

    programs.rofi = {
      enable = true;
      plugins = with pkgs; [ rofi-calc rofi-emoji ];
      extraConfig = {
        	modes = "combi";
	        show-icons = true;
	        combi-modes = "run,drun,emoji";
          # timeout = {
          #   action = "kb-cancel";
          #   delay = 0;
          # };

          # filebrowser = {
          #   directories-first = true;
          #   sorting-method = "name";
          # };
      };
      theme = "Arc";
    };

    services.blueman-applet.enable = true;
    services.network-manager-applet.enable = true;

    services.swayosd = {
      enable = true;
    };
  xdg.configFile."swayosd/style.css".text = ''
    window {
        padding: 0px 10px;
        border-radius: 25px;
        border: 10px;
        background: alpha(#282828, 0.99);
    }

    #container {
        margin: 15px;
    }

    image, label {
        color: #FBF1C7;
    }

    progressbar:disabled,
    image:disabled {
        opacity: 0.95;
    }

    progressbar {
        min-height: 6px;
        border-radius: 999px;
        background: transparent;
        border: none;
    }
    trough {
        min-height: inherit;
        border-radius: inherit;
        border: none;
        background: alpha(#DDDDDD, 0.2);
    }
    progress {
        min-height: inherit;
        border-radius: inherit;
        border: none;
        background: #FBF1C7;
    }
  '';
    services.swayidle = {
      enable = true;
      events = [
        { event = "before-sleep"; command = "bash ~/.dotfiles/home/config/i3/i3lock.sh"; }
      ];
      timeouts = [
        { timeout = 300; command = "bash ~/.dotfiles/home/config/i3/i3lock.sh"; }
        { timeout = 600; command = "swaymsg \"output * dpms off\"' resume 'swaymsg \"output * dpms on\""; }
      ];
    };

    services.dunst = {
      enable = true;
      settings = {
        global = {
          width = 300;
          height = 300;
          offset = "30x50";
          origin = "top-right";
          transparency = 10;
          frame_color = "#aaaaaa";
          frame_width = 3;
          font = "Monospace 8";
          markup = "full";
          format = "<b>%s</b>\n%b";
        };

        urgency_low = {
          background = "#35383b";
          foreground = "#fcf7e2";
          frame_color = "#27292c";
          timeout = 3;
        };

        urgency_normal = {
          background = "#35383b";
          foreground = "#dffebe";
          frame_color = "#27292c";
          timeout = 10;
        };

        urgency_critical = {
          background = "#35383b";
          foreground = "#d27c7b";
          frame_color = "#27292c";
          timeout = 0;
        };

        low_battery = {
          stack_tag = "battery";
          icon = "battery-empty-charging";
          format = "<b>%s</b> %b";
          # script = "hey .play-sound notify-critical";
      };
      };
    };

    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      systemd.enable = false;
      package = null;

      checkConfig = false;
      config = {
          input = {
              "type:keyboard" = {
                  xkb_layout = "custom,ir";
                  xkb_options = "grp:lwin_toggle";
                xkb_numlock = "enabled";
              };
              "type:touchpad" = {
                  tap = "enabled"; # no click needed
                  dwt = "enabled"; # disable touchpad while typing
                  middle_emulation = "enabled"; # natural_scroll enabled
              };
        };
        bars = [];
        keybindings = {};
        modes = {};
      };
      extraConfig = "include '~/.dotfiles/home/config/sway/config'";
    };

    home.file = {
      # "rofi".source = ~/.dotfiles/home/config/rofi;
      # "rofi".target = ".config/rofi/";

      # "rofimoji".source = ~/.dotfiles/home/config/rofimoji.rc;
      # "rofimoji".target = ".config/rofimoji.rc";

      "swappy".source = ~/.dotfiles/home/config/swappy;
      "swappy".target = ".config/swappy/";

      "gammastep".source = ~/.dotfiles/home/config/gammastep;
      "gammastep".target = ".config/gammastep/";
      
      "waybar".source = ~/.dotfiles/home/config/waybar;
      "waybar".target = ".config/waybar/";

      # "dunst".source = ~/.dotfiles/home/config/dunst;
      # "dunst".target = ".config/dunst/";

      "xkb".source = ~/.dotfiles/home/xkb;
      "xkb".target = ".xkb/";
    };
}