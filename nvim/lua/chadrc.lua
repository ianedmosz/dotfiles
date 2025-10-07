-- This file needs to have same structure as nvconfig.lua 
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :( 

---@class ChadrcConfig
local M = {}

M.plugins = {
  builtins = {
      "2html_plugin",
      "getscript",
      "getscriptPlugin",
      "gzip",
      "logipat",
      "matchit",
      "tar",
      "tarPlugin",
      "rrhelper",
      "spellfile_plugin",
      "vimball",
      "vimballPlugin",
      "zip",
      "zipPlugin",
   }
}
M.base46 = {
	theme = "onedark",

	-- hl_override = {
	-- 	Comment = { italic = true },
	-- 	["@comment"] = { italic = true },
	-- },
}

-- M.nvdash = { load_on_startup = true }
-- M.ui = {
--       tabufline = {
--          lazyload = false
--      }
-- }

vim.g.loaded_netrw = nil
vim.g.loaded_netrwPlugin = nil
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3


return M
