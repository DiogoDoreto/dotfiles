return {
  after = "nvim-treesitter",
  cmd = {
    "STSSwapUpNormal",
    "STSSwapDownNormal",
    "STSSelectCurrentNode",
    "STSSelectMasterNode",
    "STSSelectParentNode",
    "STSSelectChildNode",
    "STSSelectPrevSiblingNode",
    "STSSelectNextSiblingNode",
    "STSSwapNextVisual",
    "STSSwapPrevVisual",
  },
  config = function()
    require("syntax-tree-surfer").setup {
      highlight_group = "HopNextKey"
    }
  end,
}
