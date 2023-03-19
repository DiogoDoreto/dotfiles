return {
  { "dag/vim-fish" },
  { "jose-elias-alvarez/typescript.nvim" },
  {
    "williamboman/mason-lspconfig.nvim",
    opts = {
      ensure_installed = {
        "angularls",
        "cssls",
        "emmet_ls",
        "eslint",
        "html",
        "jsonls",
        "groovyls",
        "pyright",
        "sumneko_lua",
        "tsserver",
        "vimls",
      },
    },
  },
  {
    "jay-babu/mason-null-ls.nvim",
    opts = {
      ensure_installed = {
        "prettierd",
        "fish",
      },
    },
  },
}
