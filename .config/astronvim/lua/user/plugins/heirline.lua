return {
  "rebelot/heirline.nvim",
  opts = function(_, baseConfig)
    local status = require("astronvim.utils.status")
    local statusLine = baseConfig.statusline

    -- remove right padding from first mode block
    table.remove(statusLine[1], #statusLine[1])
    -- print cwd dir
    table.insert(statusLine, 2, { provider = vim.fs.basename(vim.fn.getcwd()) .. "/ " })
    -- replace astro's ruler
    statusLine[#statusLine - 1] = { provider = " %3l/%L:%-2c %3p%%" }

    local winbar = baseConfig.winbar
    winbar[#winbar] = {
      status.component.breadcrumbs({ hl = status.hl.get_attributes("winbar", true) }),
      status.component.fill(),
      {
        provider = function()
          local filepath = vim.fn.expand("%")
          return string.gsub(filepath, "^" .. vim.loop.os_homedir(), "~")
        end,
        update = "BufEnter",
      },
    }

    return baseConfig
  end,
}
