return {
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    config = function()
      require("toggleterm").setup({
        size = 15,
        open_mapping = [[<leader>tt]],
        hide_numbers = true,
        shade_terminals = true,
        shading_factor = 2,
        direction = "float",
        float_opts = {
          border = "rounded",
        },
      })
    end,
  },
}
