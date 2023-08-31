return {
  "hrsh7th/nvim-cmp",
  opts = function(_, opts)
    local cmp = require("cmp")
    local snip_status_ok, luasnip = pcall(require, "luasnip")
    if not snip_status_ok then return end

    -- remove possibility of using <Tab> for selecting next item in the list
    opts.mapping["<Tab>"] = cmp.mapping(function(fallback)
      if luasnip.jumpable(1) then -- prefer jumping over expanding
        luasnip.jump(1)
      elseif luasnip.expandable() then
        luasnip.expand()
      else
        fallback()
      end
    end, { "i", "s" })

    -- remove possibility of using <S-Tab> for selecting prev item in the list
    opts.mapping["<S-Tab>"] = cmp.mapping(function(fallback)
      if luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" })
  end,
}
