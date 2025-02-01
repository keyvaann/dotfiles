{ inputs, globals, ... }:
inputs.nixpkgs.lib.nixosSystem rec {
  system = "x86_64-linux";
  specialArgs = {
    firefox-addons = inputs.firefox-addons.packages.${system};
  };
  modules = [
    globals
    inputs.home-manager.nixosModules.home-manager
    ../../modules/linux
    {
      home-manager.users.${globals.user}.imports = [
        inputs.sops-nix.homeManagerModules.sops
        inputs.nix-index-database.hmModules.nix-index
      ];
    }
  ];
}