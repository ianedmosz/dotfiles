vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46/"
vim.g.mapleader = " "

-- FORCE ENABLE NETRW - Add this section here!
vim.g.loaded_netrw = 0 -- Set to 0 to ensure it loads (nil or 0 both work)
vim.g.loaded_netrwPlugin = 0
-- Optional netrw settings
vim.g.netrw_banner = 0 -- Hide banner
vim.g.netrw_liststyle = 3 -- Tree view
vim.g.netrw_browse_split = 0 -- Open in same window
vim.g.netrw_winsize = 25 -- Window size

-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  {
    "NvChad/NvChad",
    lazy = false,
    branch = "v2.5",
    import = "nvchad.plugins",
  },

  { import = "plugins" },
}, lazy_config)

-- load theme
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require "options"
require "autocmds"
require "custom.init"

vim.schedule(function()
  require "mappings"
  require "configs.netrw"
end)

-- Add netrw keybindings
vim.keymap.set("n", "<leader>E", ":edit .<CR>", { desc = "Open netrw" })
vim.keymap.set("n", "<leader>e", ":Vexplore<CR>", { desc = "Open netrw split" })

require("configs.indent")
vim.keymap.set("n", "<leader>fi", "gg=G``", { desc = "Fix indentación" })
