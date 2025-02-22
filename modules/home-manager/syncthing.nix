{ config, lib, pkgs, inputs, nixGL, ...}:
{
    services.syncthing = {
        enable = true;
        guiAddress = "127.0.0.1:8385";

        settings = {
            devices = {
                "phone" = { id = "7SGN43B-LEYIINY-EOGL2NW-VKFFGDH-UWAGM6N-X7W3THZ-YHTOYHB-4EBIFAT"; };
      	        "home-framework" = { id = "QLOBZ7E-Q4BNN3N-4YGR7RI-HTIKNID-2LXCBGU-TGFE53D-EMRXXDH-OXQELAS"; };
            };
            folders = {
                "obsidian" = {
                    path = "/home/k1/Obsidian";
                    devices = [ "home-framework" "phone" ];
                };
            };
        };
    };
}