return {
  after = "mason-lspconfig.nvim",
  config = function()
    local settings = astronvim.lsp.server_settings "tsserver"
    local default_on_attach = settings.on_attach
    settings.on_attach = function(client, bufnr)
      if client.name == "tsserver" then
        client.server_capabilities.documentFormattingProvider = false
      end
      default_on_attach(client, bufnr)
    end
    require("typescript").setup {
      server = settings
    }
  end,
}
