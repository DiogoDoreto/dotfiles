return {
  {
    "echasnovski/mini.files",
    version = false,
    opts = {},
    config = function()
      local MiniFiles = require("mini.files")

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesWindowOpen",
        callback = function(args)
          local win_id = args.data.win_id
          vim.api.nvim_win_set_config(win_id, { border = "rounded" })
        end,
      })

      local yank_fs_prop = function(prop)
        return function()
          local is_relative = prop == "relative"
          if is_relative then prop = "path" end
          local value = MiniFiles.get_fs_entry()[prop]
          if is_relative then
            local cwd = vim.fn.getcwd()
            if vim.startswith(value, cwd) then value = string.sub(value, #cwd + 2) end
          end
          vim.notify(value)
        end
      end

      local hovered_dir = function()
        local fs_entry = MiniFiles.get_fs_entry()
        if fs_entry.fs_type == "file" then
          return vim.fs.dirname(fs_entry.path)
        else
          return fs_entry.path
        end
      end

      local find_files = function()
        local dir = hovered_dir()
        MiniFiles.close()
        require("telescope.builtin").find_files({ cwd = dir })
      end

      local grep_files = function()
        local dir = hovered_dir()
        MiniFiles.close()
        require("telescope").extensions.live_grep_args.live_grep_args({ cwd = dir })
      end

      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesBufferCreate",
        callback = function(args)
          local set_keymap = function(key, desc, action)
            vim.keymap.set("n", key, action, { desc = desc, buffer = args.data.buf_id })
          end

          set_keymap("<esc>", "close MiniFiles", function() MiniFiles.close() end)
          set_keymap("<cr>", "open plus", function()
            local was_file = MiniFiles.get_fs_entry().fs_type == "file"
            MiniFiles.go_in()
            if was_file then
              local buf_id = vim.api.nvim_win_get_buf(MiniFiles.get_target_window())
              vim.bo[buf_id].buflisted = true
              MiniFiles.close()
            end
          end)

          vim.keymap.set("n", "<leader>ff", find_files, { desc = "Find files", buffer = args.data.buf_id })
          vim.keymap.set("n", "<leader>fw", grep_files, { desc = "Grep words", buffer = args.data.buf_id })

          vim.keymap.set("n", "gyn", yank_fs_prop("name"), { desc = "Yank file name", buffer = args.data.buf_id })
          vim.keymap.set("n", "gyp", yank_fs_prop("path"), { desc = "Yank full path", buffer = args.data.buf_id })
          vim.keymap.set(
            "n",
            "gyr",
            yank_fs_prop("relative"),
            { desc = "Yank relative path", buffer = args.data.buf_id }
          )
        end,
      })
    end,
  },
}
