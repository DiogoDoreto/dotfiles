return {
  "L3MON4D3/LuaSnip",
  config = function(plugin, opts)
    require "plugins.configs.luasnip" (plugin, opts)
    local curr_source = vim.fs.dirname(debug.getinfo(1, "S").source)
    local snip_path = vim.fs.normalize(curr_source:sub(2, #curr_source) .. "/../snippets")
    require("luasnip.loaders.from_snipmate").lazy_load { paths = { snip_path } }
  end,
}
