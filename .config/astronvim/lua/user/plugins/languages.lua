return {
  { "dag/vim-fish" },
  { "jose-elias-alvarez/typescript.nvim" },
  {
    "jose-elias-alvarez/null-ls.nvim",
    opts = function(_, config)
      local null_ls = require("null-ls")
      config.sources = {
        null_ls.builtins.formatting.stylua,
      }
      return config
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      auto_install = true,
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    opts = {
      ensure_installed = {
        "angularls",
        "cssls",
        "emmet_ls",
        "eslint",
        "groovyls",
        "html",
        "jsonls",
        "lua_ls",
        "pyright",
        "tsserver",
        "vimls",
      },
    },
  },
  {
    "jay-babu/mason-null-ls.nvim",
    opts = {
      ensure_installed = {
        "fish",
        "prettierd",
        "stylua",
      },
    },
  },
}
