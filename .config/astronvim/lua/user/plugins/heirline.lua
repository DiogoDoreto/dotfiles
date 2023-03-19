return {
  "rebelot/heirline.nvim",
  opts = function(_, baseConfig)
    local statusLine = baseConfig.statusline

    -- remove vim mode from beginning and end
    table.remove(statusLine, 1)
    table.remove(statusLine, #statusLine)
    -- remove scrollbar
    local ruler = { provider = " %3l/%L:%-3c %3p%%" }
    statusLine[#statusLine] = ruler
    -- print cwd dir
    table.insert(statusLine, 1, { provider = vim.fs.basename(vim.fn.getcwd()) .. "/ " })

    return baseConfig
  end,
}
