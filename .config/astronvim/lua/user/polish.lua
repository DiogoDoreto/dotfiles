return function()
  vim.api.nvim_create_autocmd("User", {
    pattern = "TelescopePreviewerLoaded",
    desc = "Show line numbers in Telescope previewer",
    callback = function()
      vim.opt_local.number = true
    end
  })
end
