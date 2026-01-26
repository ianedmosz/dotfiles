-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
--
--
vim.opt.tabstop = 4 -- A TAB character looks like 4 spaces
vim.opt.expandtab = true -- Pressing the TAB key will insert spaces instead of a TAB character
vim.opt.softtabstop = 4 -- Number of spaces inserted/removed for <BS> and <Tab> in insert mode
vim.opt.shiftwidth = 4 -- Number of spaces used for auto-indentation (e.g., when using '==')

vim.opt.fillchars:append({ eob = "~" })

vim.g.snacks_animate = false
