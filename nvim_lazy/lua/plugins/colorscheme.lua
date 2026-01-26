return {
  {
    "bluz71/vim-moonfly-colors",
    name = "moonfly",
    lazy = false,
    priority = 1000,
    config = function()
      -- Opciones del tema
      vim.g.moonflyItalics = false
      vim.g.moonflyTransparent = false
      vim.g.moonflyTerminalColors = true
      vim.g.moonflyNormalFloat = true
      vim.g.moonflyNormalPmenu = true

      vim.opt.termguicolors = true

      vim.cmd("colorscheme moonfly")
    end,
  },
}
