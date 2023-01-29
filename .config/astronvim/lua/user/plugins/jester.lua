return {
  module = "jester",
  config = function()
    require("jester").setup {
      cmd = 'yarn jest -t "$result" $file',
      path_to_jest_run = "yarn jest",
    }
  end,
}
