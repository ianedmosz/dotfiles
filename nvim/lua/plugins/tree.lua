return {
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { 
          "c", "cpp", "python", "lua", "vim", "vimdoc",
          "javascript", "typescript", "html", "css", "json",
          "bash", "markdown", "java", "rust", "go"
        },
        
        auto_install = true,
        
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
        
        indent = {
          enable = true,
        },
        
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<CR>",
            node_incremental = "<CR>",
            scope_incremental = "<TAB>",
            node_decremental = "<BS>",
          },
        },
      })
      
      -- ✨ ESTO ES LO IMPORTANTE ✨
      -- Prioriza semantic tokens sobre treesitter
      vim.highlight.priorities.semantic_tokens = 125
    end,
  }
}
