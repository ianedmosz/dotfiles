vim.g.mapleader =" "
vim.keymap.set("n","<leader>cd",vim.cmd.Ex)

vim.keymap.set("n", "<leader>tt", function()
  vim.cmd("split")   -- o split si la quieres horizontal
  vim.cmd("terminal")
end, { desc = "Abrir terminal" })

