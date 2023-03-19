
return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-telescope/telescope-file-browser.nvim"
  },
  opts = function(_, opts)
    local layout = require "telescope.actions.layout"

    opts.defaults.mappings.i["<C-h>"] = layout.toggle_preview

    opts.extensions = {
      file_browser = {
        auto_depth = true,
        hijack_netrw = true,
        grouped = true,
        path = "%:p:h",
      }
    }

    return opts
  end,
  config = function(...)
    require "plugins.configs.telescope"(...)
    local telescope = require "telescope"
    telescope.load_extension "file_browser"
  end,
}
