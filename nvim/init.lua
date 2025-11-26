vim.loader.enable() 
require('config.options')
require('config.keybinds')
require('config.lazy')
vim.opt.clipboard = "unnamedplus"


vim.api.nvim_create_autocmd("FileType", {
  pattern = { "c", "cpp", "python", "javascript", "typescript", "rust", "go", "php", "java" },
  callback = function()
    vim.opt_local.shiftwidth = 4
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.expandtab = true
  end,
})



