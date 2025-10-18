-- ~/.config/nvim/lua/custom/plugins.lua

local plugins = {
  -- ... other custom plugins

  {
    "nvim-lualine/lualine.nvim",
    opts = {
      -- Your custom lualine configuration goes here
      -- NvChad's defaults will be merged with this table.
      -- If you provide a table for a key (like 'sections'), it usually
      -- replaces the default entirely, so you might need to re-add defaults
      -- you want to keep.

      options = {
        -- Example: Change the theme
        theme = "auto", -- or a specific theme name
        -- Example: Change the separator
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
      },

      -- Example of overriding a section completely:
      -- If you want to customize lualine_a:
      -- sections = {
      --   lualine_a = { "mode" }, -- replaces NvChad's default lualine_a entirely
      -- },

      -- To add a component to an existing section without replacing the whole section:
      sections = {
        -- Append an emoji to the end of lualine_x
        lualine_x = { 
            'filetype', 
            { require("nvchad_ui.lualine").x.lualine_x_diagnostics, cond = require("nvchad_ui.lualine").has_diagnostic }, -- keep NvChad's diagnostics
            'filename', 
            'progress', 
            'location',
            '😄', -- your custom component
        },
      },
    },
  },

  -- ... other custom plugins
}

return plugins
