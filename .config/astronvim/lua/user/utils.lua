M = {}

function M.quick_notification(msg) vim.notify(msg, "info", { title = "AstroNvim", timeout = 0 }) end

function M.vim_opt_toggle(opt, on, off, name)
  local is_off = vim.opt[opt]:get() == off
  vim.opt[opt] = is_off and on or off
  M.quick_notification(name .. " " .. (is_off and "Enabled" or "Disabled"))
end

function M.v_count(def_val)
  return vim.v.count > 0 and vim.v.count or def_val
end

return M
