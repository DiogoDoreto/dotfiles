return {
  "ziontee113/syntax-tree-surfer",
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
  opts = {
    highlight_group = "HopNextKey"
  },
}
