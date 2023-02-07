local utils = require("user.utils")

return {
  n = {
    ["<leader>"] = {
      e = { "<cmd>RnvimrToggle<cr>", "Ranger File Manager" },
      s = {
        p = { "<cmd>Telescope projects<cr>", "Projects" },
      },
      p = {
        r = { "<cmd>AstroReload<cr>", "AstroReload" },
      },
      j = {
        name = "Hop / Jump",
        c = { "<cmd>HopChar1<cr>", "Character" },
        C = { "<cmd>HopChar2<cr>", "2 Characters" },
        l = { "<cmd>HopLine<cr>", "Line" },
        p = { "<cmd>HopPattern<cr>", "Pattern" },
        w = { "<cmd>HopWord<cr>", "Word" },
        s = { "<cmd>STSSelectMasterNode<cr>", "Surf" },
        S = { "<cmd>STSSelectCurrentNode<cr>", "Surf Node" },
      },
      t = {
        t = { "<cmd>OverseerRun<cr>", "Run task" },
        T = { "<cmd>OverseerToggle<cr>", "Toggle task list" },
        j = { function() require "jester".run() end, "Run Jest test under the cursor" },
        J = { function() require "jester".run_file() end, "Run Jest test under the cursor" },
      },
      u = {
        z = {
          function() utils.vim_opt_toggle("foldenable", true, false, "Fold") end,
          "Toggle fold",
        }
      },
      z = { "<cmd>ZenMode<cr>", "Zen Mode" }
    },
    g = {
      t = {
        name = "Treesitter",
        v = {
          function()
            require("syntax-tree-surfer").targeted_jump {
              "variable_declaration",
              "variable_declarator",
            }
          end,
          "Go to Variables",
        },
        f = {
          function() require("syntax-tree-surfer").targeted_jump { "function" } end,
          "Go to Functions",
        },
        i = {
          function()
            require("syntax-tree-surfer").targeted_jump {
              "if_statement",
              "else_clause",
              "else_statement",
              "elseif_statement",
            }
          end,
          "Go to If Statements",
        },
        r = {
          function() require("syntax-tree-surfer").targeted_jump { "for_statement" } end,
          "Go to If Statements",
        },
        w = {
          function() require("syntax-tree-surfer").targeted_jump { "while_statement" } end,
          "Go to While Statements",
        },
        s = {
          function() require("syntax-tree-surfer").targeted_jump { "switch_statement" } end,
          "Go to Switch Statements",
        },
        t = {
          function()
            require("syntax-tree-surfer").targeted_jump {
              "function",
              "if_statement",
              "else_clause",
              "else_statement",
              "elseif_statement",
              "for_statement",
              "while_statement",
              "switch_statement",
              "class_declaration",
              "method_definition",
            }
          end,
          "Go to Statement",
        },
      },
    },
  },

  v = {
    ["<leader>"] = {
      j = {
        name = "Hop / Jump",
        c = { "<cmd>HopChar1<cr>", "Character" },
        C = { "<cmd>HopChar2<cr>", "2 Characters" },
        l = { "<cmd>HopLine<cr>", "Line" },
        p = { "<cmd>HopPattern<cr>", "Pattern" },
        w = { "<cmd>HopWord<cr>", "Word" },
      },
    },
  }
}
