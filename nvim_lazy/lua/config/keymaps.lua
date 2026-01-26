-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
--
vim.keymap.set("n", "<leader>tt", function()
  vim.cmd("split") -- o split si la quieres horizontal
  vim.cmd("terminal")
end, { desc = "Open the Terminal" })

vim.keymap.set("n", "<leader>cd", vim.cmd.Ex)
