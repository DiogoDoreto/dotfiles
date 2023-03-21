return {
  "ziontee113/syntax-tree-surfer",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
  },
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
  opts = {
    highlight_group = "HopNextKey"
  },
}
