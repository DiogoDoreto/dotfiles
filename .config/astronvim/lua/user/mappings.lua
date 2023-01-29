return {
  n = {
    -- disable default bindings
    ["<C-Down>"] = false,
    ["<C-Left>"] = false,
    ["<C-Right>"] = false,
    ["<C-Up>"] = false,
    ["<C-q>"] = false,
    -- resize with arrows
    ["<Up>"] = { function() require("smart-splits").resize_up(vim.v.count > 0 and vim.v.count or 2) end, desc = "Resize split up" },
    ["<Down>"] = { function() require("smart-splits").resize_down(vim.v.count > 0 and vim.v.count or 2) end,
      desc = "Resize split down" },
    ["<Left>"] = { function() require("smart-splits").resize_left(vim.v.count > 0 and vim.v.count or 2) end, desc = "Resize split left" },
    ["<Right>"] = { function() require("smart-splits").resize_right(vim.v.count > 0 and vim.v.count or 2) end, desc = "Resize split right" },
    -- navigating wrapped lines
    j = { "gj", desc = "Navigate down" },
    k = { "gk", desc = "Navigate down" },
    -- easy linebreaks
    ["<C-CR>"] = { "o<Esc>", desc = "Add line below" },
    ["<S-C-CR>"] = { "O<Esc>", desc = "Add line above" },
    -- easy splits
    ["_"] = { "<cmd>split<cr>", desc = "Horizontal split" },
    ["|"] = { "<cmd>vsplit<cr>", desc = "Vertical split" },
    -- Treesitter Surfer
    ["<c-down>"] = { "<cmd>STSSwapDownNormal<cr>", desc = "Swap next tree-sitter object" },
    ["<c-right>"] = { "<cmd>STSSwapDownNormal<cr>", desc = "Swap next tree-sitter object" },
    ["<c-up>"] = { "<cmd>STSSwapUpNormal<cr>", desc = "Swap previous tree-sitter object" },
    ["<c-left>"] = { "<cmd>STSSwapUpNormal<cr>", desc = "Swap previous tree-sitter object" },
  },

  i = {
    -- easy linebreaks
    ["<C-CR>"] = { "<Esc>o", desc = "Add line below" },
    ["<S-C-CR>"] = { "<Esc>O", desc = "Add line above" },
  },

  v = {
    -- navigating wrapped lines
    j = { "gj", desc = "Navigate down" },
    k = { "gk", desc = "Navigate down" },
  },

  x = {
    -- Tressitter Surfer
    ["J"] = { "<cmd>STSSelectNextSiblingNode<cr>", desc = "Surf next tree-sitter object" },
    ["K"] = { "<cmd>STSSelectPrevSiblingNode<cr>", desc = "Surf previous tree-sitter object" },
    ["H"] = { "<cmd>STSSelectParentNode<cr>", desc = "Surf parent tree-sitter object" },
    ["L"] = { "<cmd>STSSelectChildNode<cr>", desc = "Surf child tree-sitter object" },
    ["<c-j>"] = { "<cmd>STSSwapNextVisual<cr>", desc = "Surf next tree-sitter object" },
    ["<c-l>"] = { "<cmd>STSSwapNextVisual<cr>", desc = "Surf next tree-sitter object" },
    ["<c-k>"] = { "<cmd>STSSwapPrevVisual<cr>", desc = "Surf previous tree-sitter object" },
    ["<c-h>"] = { "<cmd>STSSwapPrevVisual<cr>", desc = "Surf previous tree-sitter object" },
  },

  -- terminal mappings
  t = {
    ["<esc><esc>"] = { "<c-\\><c-n>", desc = "Terminal normal mode" },
    ["<c-q>"] = { "<c-\\><c-n>:q<cr>", desc = "Terminal quit" },
  },
}
