{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dog.programs.firefox;
  homedir = config.home.homeDirectory;
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
          Google = {
            id = 0;
            color = "red";
            icon = "fence";
          };
          Shopping = {
            id = 1;
            color = "green";
            icon = "dollar";
          };
        };
        # See https://nur.nix-community.org/repos/rycee/
        extensions = with config.nur.repos.rycee.firefox-addons; [
          multi-account-containers
          privacy-badger
          pwas-for-firefox
          raindropio
          ublock-origin
        ];
        settings = {
          "app.shield.optoutstudies.enabled" = true;
          "browser.aboutConfig.showWarning" = false;
          "browser.ctrlTab.sortByRecentlyUsed" = true;
          "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.urlbar.showSearchSuggestionsFirst" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "extensions.formautofill.creditCards.enabled" = false;
          "media.eme.enabled" = true;
          "privacy.donottrackheader.enabled" = true;
          "privacy.globalprivacycontrol.enabled" = true;
          "signon.rememberSignons" = false;
          "widget.gtk.overlay-scrollbars.enabled" = true;
        };
      };
    };
  };
}
