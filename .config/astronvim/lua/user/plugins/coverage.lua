return {
  "andythigpen/nvim-coverage",
  opts = {
    signs = {
      covered = { text = "" },
      uncovered = { text = "" },
      partial = { text = "" },
    },
  },
  cmd = {
    "Coverage",
    "CoverageLoad",
    "CoverageLoadLcov",
  },
}
