{ config, lib, pkgs, inputs, nixGL, ...}:
{
  programs.firefox = {
    enable = true;
    package = (config.lib.nixGL.wrap pkgs.firefox-wayland);
    profiles.default = {
      userChrome = ''
        #TabsToolbar { visibility: collapse !important; }
        #navigator-toolbox[fullscreenShouldAnimate] { transition: none !important; }
      '';

      bookmarks = [ ];

      extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
        clearurls
        sidebery
        darkreader
        # improved-tube
        # enhancer-for-youtube
        sponsorblock
        # languagetool
        augmented-steam
        decentraleyes
        kagi-search
        linkding-extension
        old-reddit-redirect
        reddit-enhancement-suite
        proton-pass
        
        
        # imagus
        ff2mpv
        istilldontcareaboutcookies
        multi-account-containers
        privacy-badger
        refined-github
        to-google-translate
        ublock-origin
        # simple-temporary-containers
        # retainer
        # container-tab-flow
        # open-external-links-in-a-container
      ];

      settings = {
        "dom.security.https_only_mode" = true;
        "dom.forms.autocomplete.formautofill" = false;
        "dom.payments.defaults.saveAddress" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.newtabpage.enabled" = false;
        "browser.startup.homepage" = "about:blank";
        "browser.zoom.full" = true;
        "browser.startup.page" = 3;
        "browser.tabs.inTitlebar" = 0;
        "browser.toolbars.bookmarks.visibility" = false;
        "browser.urlbar.trimURLs" = false;
        "browser.sessionstore.warnOnQuit" = true;
        "browser.aboutConfig.showWarning" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
        "font.size.variable.x-western"= 18;
        "extensions.pocket.enabled" = false;
        "gfx.webrender.all" = true;
        "services.sync.engine.addons" = false;
        "services.sync.engine.creditcards" = false;
        "services.sync.engine.passwords" = false;
      };
    };
  };
}