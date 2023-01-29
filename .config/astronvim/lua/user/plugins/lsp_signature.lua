return {
  event = "InsertEnter",
  config = function()
    require("lsp_signature").setup {}
  end,
}
