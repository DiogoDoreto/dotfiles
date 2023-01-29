return function(config)
  config.window.width = 35
  config.filesystem.filtered_items = {
    hide_dotfiles = false,
    hide_by_name = {
      ".git",
    },
    never_show = {
      ".DS_Store",
    },
  }

  table.insert(config.event_handlers, {
    event = "neo_tree_buffer_enter",
    handler = function(_)
      vim.opt_local.relativenumber = true
    end
  })

  return config
end
