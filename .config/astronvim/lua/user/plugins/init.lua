return {
  ["max397574/better-escape.nvim"] = { disable = true },

  -- remove in nvim 0.9
  ["gpanders/editorconfig.nvim"] = {},

  ["jose-elias-alvarez/typescript.nvim"] = require "user.plugins.typescript",

  ["kylechui/nvim-surround"] = { config = function() require "nvim-surround".setup() end },

  ["ahmedkhalf/project.nvim"] = require "user.plugins.project",

  ["ziontee113/syntax-tree-surfer"] = require "user.plugins.syntax-tree-surfer",

  ["phaazon/hop.nvim"] = require "user.plugins.hop",

  ["ray-x/lsp_signature.nvim"] = require "user.plugins.lsp_signature",

  ["David-Kunz/jester"] = require "user.plugins.jester",

  ["folke/zen-mode.nvim"] = require "user.plugins.zen-mode",

  ["EdenEast/nightfox.nvim"] = require "user.plugins.nightfox",

  ["dag/vim-fish"] = {},

  ["microsoft/vscode-js-debug"] = require "user.plugins.vscode-js-debug",

  ["mxsdev/nvim-dap-vscode-js"] = require "user.plugins.nvim-dap-vscode-js",

  ["stevearc/overseer.nvim"] = require "user.plugins.overseer",

  ["klen/nvim-config-local"] = { config = function() require "config-local".setup() end },
}
