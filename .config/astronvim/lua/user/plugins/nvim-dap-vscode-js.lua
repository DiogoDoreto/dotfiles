return {
  opt = true,
  after = "nvim-dap",
  config = function()
    require("dap-vscode-js").setup({
      adapters = { "pwa-node", "pwa-chrome" },
    })

    for _, language in ipairs({ "typescript", "typescriptreact" }) do
      require("dap").configurations[language] = {
        {
          name = "Chrome: Debug",
          type = "pwa-chrome",
          request = "attach",
          program = "${file}",
          cwd = vim.fn.getcwd(),
          sourceMaps = true,
          protocol = "inspector",
          port = 9222,
          webRoot = "${workspaceFolder}",
        }
      }
    end
  end
}
