return {
  "sindrets/diffview.nvim",
  event = "User AstroGitFile",
  cmd = {
    "DiffviewOpen",
    "DiffviewFileHistory",
  },
  opts = function()
    local actions = require("diffview.actions")
    return {
      keymaps = {
        view = {
          ["<leader>b"] = false,
          { "n", "<leader>o", actions.toggle_files, { desc = "Toggle the file panel." } },
        },
        file_panel = {
          ["<leader>b"] = false,
          { "n", "<leader>o", actions.toggle_files, { desc = "Toggle the file panel." } },
        },
        file_history_panel = {
          ["<leader>b"] = false,
          { "n", "<leader>o", actions.toggle_files, { desc = "Toggle the file panel." } },
        },
      }
    }
  end,
}
