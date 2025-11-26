-- This file needs to have same structure as nvconfig.lua 
-- https://github.com/NvChad/ui/blob/v3.0/lua/nvconfig.lua
-- Please read that file to know all available options :( 

---@class ChadrcConfig
local M = {}
local p = require("catppuccin.palettes").get_palette "mocha"
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
  theme = "catppuccin",    -- usa la theme catppuccin incluida en base46
  hl_override = {
    -- ejemplos de overrides por nombre de highlight group
    -- usa nombres de variables del tema (base_30) como "one_bg", "blue", "white", etc.
    --
    BufferLineFill = { bg = "one_bg" },
    BufferLineBackground = { fg = "grey", bg = "one_bg" },
    BufferLineBufferSelected = { fg = "white", bg = "blue" },
    BufferLineTabSeparator = { fg = "one_bg2", bg = "one_bg" },
    BufferLineIndicatorSelected = { fg = "green", bg = "none" },
  },
}


vim.g.loaded_netrw = nil
vim.g.loaded_netrwPlugin = nil
vim.g.netrw_banner = 0
vim.g.netrw_liststyle = 3


M.ui = {
  theme = "catppuccin", -- o el que uses
  tabufline = {
    enabled = true,
    lazyload = false,
    overriden_modules = nil,
  },
  hl_override = {
    TabufLine = { bg = "#1E1E2E", fg = "#BAC2DE" },
    TabufLineSel = { bg = "#89B4FA", fg = "#1E1E2E", bold = true },
    TabufLineFill = { bg = "#11111B" },
  },
}


M.setup = function()
  require("lualine").setup({
    options = {
      theme = "auto",
      component_separators = "|",
      section_separators = "",
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = { "branch", "diff", "diagnostics" },
      lualine_c = {
        "filename",
        {
          function()
            -- Mostrar el contexto de treesitter
            local context = vim.b.treesitter_context
            if context and context ~= "" then
              return " " .. context
            end
            return ""
          end,
          color = { fg = "#98c379" },
        },
      },
      lualine_x = { "encoding", "fileformat", "filetype" },
      lualine_y = { "progress" },
      lualine_z = { "location" },
    },
  })
end




return M
