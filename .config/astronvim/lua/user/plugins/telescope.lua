local layout = require "telescope.actions.layout"

return {
  defaults = {
    mappings = {
      i = {
        ["<C-h>"] = layout.toggle_preview,
      },
    }
  },
}
