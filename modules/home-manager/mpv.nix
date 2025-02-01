{ config, lib, pkgs, inputs, nixGL, ...}:
{ 
  programs.mpv = {
    enable = true;
    package = (config.lib.nixGL.wrap pkgs.mpv);
    
    config = {
      image-display-duration = 2; # For cycling through images
      hwdec = "auto-safe"; # Attempt to use GPU decoding for video
      sub-auto = "all";
      watch-later-options-clr = true; # Dont save settings like brightness
      vo = "gpu"; # uses gpu-accelerated video output by default.
    };

    extraInput = ''
      MOUSE_BTN3 add volume 1
      MOUSE_BTN4 add volume -1
      PGUP seek 600
      PGDWN seek -600

      Z add sub-delay -0.1
      X add sub-delay +0.1

      z sub-step -1
      x sub-step +1

      ENTER playlist_next force              # skip to next file or quit

      R rescan-external-files
      ' quit
      گ quit
      ض quit
    '';
  };
  xdg.configFile = with pkgs; {
    "mpv/scripts/autoload.lua".source =
      pkgs.fetchFromGitHub {
        owner = "mpv-player";
        repo = "mpv";
        rev = "59d1dc43b963a03cbaa8198d6c92105b67f86c7c";
        sha256 = "sha256-W470tQUBW/HZIWS/DAbkmmUrcd5a+HYruucW/8aYvuY=";
      }
      + "/TOOLS/lua/autoload.lua"; 
  };
}