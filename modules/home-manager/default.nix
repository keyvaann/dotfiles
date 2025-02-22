# Add your reusable home-manager modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  # List your module files here
  firefox = import ./firefox.nix;
  mime = import ./mime.nix;
  mpv = import ./mpv.nix;
  desktop = import ./desktop.nix;
  shell = import ./shell.nix;
  fonts = import ./fonts.nix;
  git = import ./git.nix;
  syncthing = import ./syncthing.nix;
}
