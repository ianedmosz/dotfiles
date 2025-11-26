return{

  -- En tu plugins.lua, agrega:
{
  "nvim-treesitter/nvim-treesitter-context",
  dependencies = "nvim-treesitter/nvim-treesitter",
  config = function()
    require("treesitter-context").setup({
      enable = true,
      max_lines = 3,
      min_window_height = 0,
      line_numbers = true,
      multiline_threshold = 1,
      trim_scope = 'outer',
      mode = 'cursor',
      separator = nil,
    })
  end,
}

}
