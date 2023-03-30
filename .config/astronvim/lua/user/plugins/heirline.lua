return {
  "rebelot/heirline.nvim",
  opts = function(_, baseConfig)
    local status = require("astronvim.utils.status")
    local statusLine = baseConfig.statusline

    -- remove vim mode from beginning and end
    table.remove(statusLine, 1)
    table.remove(statusLine, #statusLine)
    -- remove scrollbar
    local ruler = { provider = " %3l/%L:%-3c %3p%%" }
    statusLine[#statusLine] = ruler
    -- print cwd dir
    table.insert(statusLine, 1, { provider = vim.fs.basename(vim.fn.getcwd()) .. "/ " })

    local winbar = baseConfig.winbar
    winbar[#winbar] = {
      status.component.breadcrumbs({ hl = status.hl.get_attributes("winbar", true) }),
      status.component.fill(),
      {
        provider = function()
          return vim.fn.expand("%")
        end,
        update = "BufEnter",
      },
    }

    return baseConfig
  end,
}
