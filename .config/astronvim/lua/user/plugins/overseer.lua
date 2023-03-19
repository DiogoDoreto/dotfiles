return {
  "stevearc/overseer.nvim",
  opts = {
    task_list = {
      bindings = {
        -- disable keymaps already in use
        ["<C-h>"] = false,
        ["<C-l>"] = false,
        ["<H>"] = false,
        ["<L>"] = false,
        -- remap lost behavior
        ["<C-a>"] = "IncreaseDetail",
        ["<C-x>"] = "DecreaseDetail",
      },
    },
  },
}
