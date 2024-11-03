{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.firefox;
in {
  options.dog.programs.firefox = {
    enable = mkEnableOption "Firefox";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      firefoxpwa
    ];

    programs.firefox = {
      enable = true;
      nativeMessagingHosts = [ pkgs.firefoxpwa ];
      profiles.dog = {
        id = 0;
        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
        };
        containersForce = true;
        containers = {
          Default = {
            id = 0;
            color = "toolbar";
            icon = "circle";
          };
          Google = {
            id = 1;
            color = "red";
            icon = "fence";
          };
          Shopping = {
            id = 2;
            color = "green";
            icon = "dollar";
          };
          Facebook = {
            id = 10;
            color = "blue";
            icon = "fence";
          };
        };
        # See https://nur.nix-community.org/repos/rycee/
        extensions = with config.nur.repos.rycee.firefox-addons; [
          multi-account-containers
          privacy-badger
          pwas-for-firefox
          raindropio
          ublock-origin
          vimium
        ];
        # updates ~/.mozilla/firefox/dog/prefs.js
        settings = {
          "app.shield.optoutstudies.enabled" = false;
          "browser.aboutConfig.showWarning" = false;
          "browser.aboutwelcome.didSeeFinalScreen" = true;
          "browser.ctrlTab.sortByRecentlyUsed" = true;
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.blocked" = "{\"mZmevP23jfB3rScn/QCWnw==\":1,\"26UbzFJ7qT9/4DhodHKA1Q==\":1,\"4gPpjkxgZzXPVtuEoAL9Ig==\":1,\"eV8/WsSLxHadrTL1gAxhug==\":1,\"gLv0ja2RYVgxKdp0I5qwvA==\":1,\"T9nJot5PurhJSy8n038xGA==\":1}";
          "browser.newtabpage.pinned" = "[]";
          "browser.urlbar.showSearchSuggestionsFirst" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "extensions.formautofill.creditCards.enabled" = false;
          "media.eme.enabled" = true;
          "privacy.donottrackheader.enabled" = true;
          "privacy.globalprivacycontrol.enabled" = true;
          "signon.rememberSignons" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "widget.gtk.overlay-scrollbars.enabled" = true;
        };
        userChrome = let
          cssHacks = pkgs.fetchFromGitHub {
            owner = "MrOtherGuy";
            repo = "firefox-csshacks";
            rev = "ec1aede46c2640d4daff491539c2637e27b07bdc";
            sha256 = "n/hYUV8HIzf76S1v06OsVhft2J+jYUBbN53UwsQxzGw=";
          };
        in readFile "${cssHacks}/chrome/hide_tabs_with_one_tab.css";
      };
    };
  };
}
