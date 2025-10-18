return {
  {
    "olimorris/onedarkpro.nvim",
    priority = 1000,
    config = function()
      require("onedarkpro").setup({
        highlights = {
          -- ✨ SOLO VARIABLES EN ROJO ✨
          ["@lsp.type.variable"] = { fg = "${red}" },
          ["@lsp.type.variable.c"] = { fg = "${red}" },
          ["@lsp.type.variable.cpp"] = { fg = "${red}" },
          ["@lsp.type.variable.python"] = { fg = "${red}" },
          ["@lsp.type.variable.rust"] = { fg = "${red}" },
          ["@lsp.type.variable.java"] = { fg = "${red}" },
          ["@lsp.type.variable.lua"] = { fg = "${red}" },
        },
        
        plugins = {
          nvim_lsp = true,
          nvim_tree = true,
          nvim_cmp = true,
          gitsigns = true,
          telescope = true,
          treesitter = true,
          nvim_navic = true,
          lsp_semantic_tokens = true,
        },
        
        styles = {
          comments = "italic",
          keywords = "bold,italic",
          functions = "italic",
          conditionals = "italic",
        },
        
        options = {
          transparency = false,
          terminal_colors = true,
        },
      })
      
      vim.cmd("colorscheme onedark")
    end,
  },
}
