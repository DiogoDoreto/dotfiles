return {
  "mickael-menu/zk-nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  main = "zk",
  opts = {
    picker = "telescope",
  },
  cmd = {
    "ZkIndex",
    "ZkNew",
    "ZkNewFromTitleSelection",
    "ZkNewFromContentSelection",
    "ZkCd",
    "ZkNotes",
    "ZkBacklinks",
    "ZkLinks",
    "ZkInsertLink",
    "ZkInsertLinkAtSelection",
    "ZkMatch",
    "ZkTags",
  },
}
