-- lua/custom/plugins/treesitter.lua
return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  config = function()
    require'nvim-treesitter.configs'.setup {
      ensure_installed = { "c", "cpp", "lua", "python", "java", "javascript" },
      highlight = { enable = true },
      indent = { enable = true },  -- esto activa la indentación automática
    }
  end
}

