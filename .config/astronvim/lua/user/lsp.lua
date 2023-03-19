return {
  setup_handlers = {
    tsserver = function(_, opts) require("typescript").setup { server = opts } end
  },
  formatting = {
    filter = function (client)
      if client.name == "tsserver" then
        return false
      end
      return true
    end
  }
}
