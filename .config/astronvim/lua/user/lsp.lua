return {
  setup_handlers = {
    tsserver = function(_, opts) require("typescript").setup { server = opts } end
  },
  formatting = {
    disabled = {
      "lua_ls",
      "tsserver",
    }
  }
}
