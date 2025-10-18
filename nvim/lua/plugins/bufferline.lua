return {
  "akinsho/bufferline.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  -- Es crucial que se cargue DESPUÉS de Catppuccin
  after = "catppuccin", 
  
  config = function()
    local catppuccin = require("catppuccin.palettes")
    local p = catppuccin.get_palette "mocha" 

    require("bufferline").setup {
        options = {
             separator_style = "slant",
             always_show_bufferline = false, -- Muestra la barra solo con varios buffers
        }
    }

    
    vim.api.nvim_set_hl(0, "BufferLineFill", { fg = p.mantle, bg = p.mantle }) 
    
    vim.api.nvim_set_hl(0, "BufferLineBuffer", { fg = p.subtext0, bg = p.mantle })
    vim.api.nvim_set_hl(0, "BufferLineBackground", { fg = p.subtext0, bg = p.mantle })
    
    vim.api.nvim_set_hl(0, "BufferLineSelected", { fg = p.text, bg = p.crust, bold = true }) 
    
    vim.api.nvim_set_hl(0, "BufferLineIndicatorSelected", { sp = p.mauve, underline = true })
    
    vim.api.nvim_set_hl(0, "BufferLineCloseSelected", { fg = p.red, bg = p.crust })
    vim.api.nvim_set_hl(0, "BufferLineClose", { fg = p.subtext1, bg = p.mantle })
    
  end,
}
