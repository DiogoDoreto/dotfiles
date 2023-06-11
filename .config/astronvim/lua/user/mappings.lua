-- ignore formatting in this file, as one-liners are easier to read
-- stylua: ignore start
local utils = require("user.utils")
local zkCmd = require("user.commands.zk")

local maps = { i = {}, n = {}, v = {}, t = {}, x = {} }

maps.n["<leader>j"] = { name = "Hop / Jump" }
maps.v["<leader>j"] = { name = "Hop / Jump" }
maps.n["<leader>x"] = { name = "Trouble" }
maps.n["<leader>z"] = { name = "Zk Notes" }
maps.v["<leader>z"] = { name = "Zk Notes" }

-- disable default bindings
maps.n["<C-Down>"] = false
maps.n["<C-Left>"] = false
maps.n["<C-Right>"] = false
maps.n["<C-Up>"] = false
maps.n["<C-q>"] = false

-- quick expand known snippets
maps.i["<C-Tab>"] = { function() require("luasnip").expand() end }

maps.n["<Leader>bx"] = { "<cmd>tabclose<cr>", desc = "Close tab" }
maps.n["<leader>o"] = { "<cmd>Telescope file_browser<cr>", desc = "Telescope file browser" }

maps.n["<leader>fw"] = {
  function() require("telescope").extensions.live_grep_args.live_grep_args() end,
  desc = "Find words",
}

-- switch tabs with H and L
maps.n.L = { function() require("astronvim.utils.buffer").nav(utils.v_count(1)) end, desc = "Next buffer" }
maps.n.H = { function() require("astronvim.utils.buffer").nav(-(utils.v_count(1))) end, desc = "Previous buffer" }

-- resize with arrows
maps.n["<Up>"] = { function() require("smart-splits").resize_up(utils.v_count(2)) end, desc = "Resize split up" }
maps.n["<Down>"] = { function() require("smart-splits").resize_down(utils.v_count(2)) end, desc = "Resize split down" }
maps.n["<Left>"] = { function() require("smart-splits").resize_left(utils.v_count(2)) end, desc = "Resize split left" }
maps.n["<Right>"] = { function() require("smart-splits").resize_right(utils.v_count(2)) end, desc = "Resize split right" }

-- easy linebreaks
maps.n["<C-CR>"] = { "o<Esc>", desc = "Add line below" }
maps.n["<S-C-CR>"] = { "O<Esc>", desc = "Add line above" }
maps.i["<C-CR>"] = { "<Esc>o", desc = "Add line below" }
maps.i["<S-C-CR>"] = { "<Esc>O", desc = "Add line above" }

-- Git
maps.n["<Leader>gt"] = { "<cmd>DiffviewOpen<cr>", desc = "Git status" }
maps.n["<Leader>gd"] = { ":DiffviewOpen ", desc = "Git diff" }
maps.n["<Leader>gL"] = { ":DiffviewFileHistory --range=", desc = "Git log" }

-- Treesitter Surfer
maps.n["<c-down>"] = { "<cmd>STSSwapDownNormal<cr>", desc = "Swap next tree-sitter object" }
maps.n["<c-right>"] = { "<cmd>STSSwapDownNormal<cr>", desc = "Swap next tree-sitter object" }
maps.n["<c-up>"] = { "<cmd>STSSwapUpNormal<cr>", desc = "Swap previous tree-sitter object" }
maps.n["<c-left>"] = { "<cmd>STSSwapUpNormal<cr>", desc = "Swap previous tree-sitter object" }
maps.n["<leader>js"] = { "<cmd>STSSelectMasterNode<cr>", desc = "Surf" }
maps.n["<leader>jS"] = { "<cmd>STSSelectCurrentode<cr>", desc = "Surf Node" }
maps.x["J"] = { "<cmd>STSSelectNextSiblingNode<cr>", desc = "Surf next tree-sitter object" }
maps.x["K"] = { "<cmd>STSSelectPrevSiblingNode<cr>", desc = "Surf previous tree-sitter object" }
maps.x["H"] = { "<cmd>STSSelectParentNode<cr>", desc = "Surf parent tree-sitter object" }
maps.x["L"] = { "<cmd>STSSelectChildNode<cr>", desc = "Surf child tree-sitter object" }
maps.x["<c-j>"] = { "<cmd>STSSwapNextVisual<cr>", desc = "Surf next tree-sitter object" }
maps.x["<c-l>"] = { "<cmd>STSSwapNextVisual<cr>", desc = "Surf next tree-sitter object" }
maps.x["<c-k>"] = { "<cmd>STSSwapPrevVisual<cr>", desc = "Surf previous tree-sitter object" }
maps.x["<c-h>"] = { "<cmd>STSSwapPrevVisual<cr>", desc = "Surf previous tree-sitter object" }

-- Hop
maps.n["<leader>jc"] = { "<cmd>HopChar1<cr>", desc = "Character" }
maps.n["<leader>jC"] = { "<cmd>HopChar2<cr>", desc = "2 Characters" }
maps.n["<leader>jl"] = { "<cmd>HopLine<cr>", desc = "Line" }
maps.n["<leader>jp"] = { "<cmd>HopPattern<cr>", desc = "Pattern" }
maps.n["<leader>jw"] = { "<cmd>HopWord<cr>", desc = "Word" }
maps.v["<leader>jc"] = maps.n["<Leader>jc"]
maps.v["<leader>jC"] = maps.n["<Leader>jC"]
maps.v["<leader>jl"] = maps.n["<Leader>jl"]
maps.v["<leader>jp"] = maps.n["<Leader>jp"]
maps.v["<leader>jw"] = maps.n["<Leader>jw"]

-- Terminal group
maps.n["<leader>tt"] = { "<cmd>OverseerRun<cr>", desc = "Run task" }
maps.n["<leader>tT"] = { "<cmd>OverseerToggle<cr>", desc = "Toggle task list" }
maps.n["<leader>tj"] = { function() require "jester".run() end, desc = "Run Jest test under the cursor" }
maps.n["<leader>tJ"] = { function() require "jester".run_file() end, desc = "Run Jest test under the cursor" }

-- UI group
maps.n["<leader>uz"] = { function() utils.vim_opt_toggle("foldenable", true, false, "Fold") end, desc = "Toggle fold" }

-- Trouble group
maps.n["<leader>xz"] = { "<cmd>TroubleToggle<cr>", desc = "Toggle" }
maps.n["<leader>xx"] = { "<cmd>TroubleRefresh<cr>", desc = "Refresh" }
maps.n["<leader>xw"] = { "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Workspace Diagnostics" }
maps.n["<leader>xd"] = { "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Document Diagnostics" }
maps.n["<leader>xq"] = { "<cmd>TroubleToggle quickfix<cr>", desc = "QuickFix" }
maps.n["<leader>xl"] = { "<cmd>TroubleToggle loclist<cr>", desc = "LocList" }
maps.n["<leader>xr"] = { "<cmd>TroubleToggle lsp_references<cr>", desc = "LSP References" }
maps.n["<leader>xj"] = {
  function() require("trouble").next({ skip_groups = true, jump = true }) end,
  desc = "Next Item"
}
maps.n["<leader>xk"] = {
  function() require("trouble").previous({ skip_groups = true, jump = true }) end,
  desc = "Previous Item"
}

-- Zk Notes group
maps.n["<leader>zn"] = { function() require("zk").new({ title = vim.fn.input("Title: ") }) end, desc = "New note" }
maps.v["<leader>zn"] = { "<cmd>'<,'>ZkNewFromTitleSelection<cr>", desc = "New from title selection" }
maps.v["<leader>zc"] = { "<cmd>'<,'>ZkNewFromContentSelection<cr>", desc = "New from content selection" }
maps.n["<leader>zj"] = { "<cmd>ZkNew { dir = 'journal' }<cr>", desc = "New journal entry" }
maps.n["<leader>zJ"] = { zkCmd.newJiraNote, desc = "New JIRA note" }
-- maps.n["<leader>zj"] = {
--   function() require("zk").new({ dir = vim.fn.expand("$ZK_NOTEBOOK_DIR/journal") }) end,
--   desc = "New journal note"
-- }
maps.n["<leader>zb"] = { "<cmd>ZkBacklinks<cr>", desc = "Backlinks of current note" }
maps.n["<leader>zl"] = { "<cmd>ZkLinks<cr>", desc = "Links of current note" }
maps.n["<leader>zi"] = { "<cmd>ZkInsertLink<cr>", desc = "Insert link" }
maps.v["<leader>zi"] = { "<cmd>'<,'>ZkInsertLinkAtSelection<cr>", desc = "Insert link" }
maps.n["<leader>zs"] = { "<cmd>ZkNotes<cr>", desc = "Search notes" }
maps.n["<leader>zt"] = { "<cmd>ZkTags<cr>", desc = "Search tags" }

-- terminal mappings
maps.t["<esc><esc>"] = { "<c-\\><c-n>", desc = "Terminal normal mode" }
maps.t["<c-q>"] = { "<c-\\><c-n>:q<cr>", desc = "Terminal quit" }

return maps
-- stylua: ignore end
