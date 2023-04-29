local M = {}

-- Open or create a new note for a specific Jira ID. Uses the ID under the cursor otherwise asks for it.
function M.newJiraNote()
  local word = vim.fn.expand("<cWORD>")
  local search = string.match(word, "%u+-%d+")
  local id = search or vim.fn.input("Jira ID: ")

  require("zk").new({ title = id, dir = "jira" })
end

return M
