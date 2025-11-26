return {
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      {
        "mason-org/mason.nvim",
        opts = {
          ui = {
            icons = {
              package_installed = "✓",
              package_pending = "➜",
              package_uninstalled = "✗",
            },
          },
        },
      },
      "neovim/nvim-lspconfig", 
      "hrsh7th/nvim-cmp",
      "hrsh7th/cmp-nvim-lsp",
      "L3MON4D3/LuaSnip",
      "onsails/lspkind.nvim",
      "SmiteshP/nvim-navic",
      "mfussenegger/nvim-jdtls",
    },
    opts = {
      ensure_installed = {
        "lua_ls",
        "ts_ls",
        "rust_analyzer",
        "pyright",
        "clangd",
        "marksman",
      },
      automatic_installation = true,
      automatic_enable = false, 
    },
    config = function(_, opts)
      require("mason-lspconfig").setup(opts)
      require("config.lsp")
    end,
  },
}

