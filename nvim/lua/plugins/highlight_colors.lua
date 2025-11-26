return {
  {
    "brenoprata10/nvim-highlight-colors",
    lazy = true,
    config = function()
      vim.opt.termguicolors = true
      require("nvim-highlight-colors").setup({
        -- puedes pasar opciones acá, usa {} para default
      })
    end,
  },
}
