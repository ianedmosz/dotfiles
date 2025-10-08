return{
  -- En tu plugins.lua
{
  "lewis6991/hover.nvim",
  config = function()
    require("hover").setup({
      init = function()
        require("hover.providers.lsp")
        require("hover.providers.man")
        require("hover.providers.dictionary")
      end,
      preview_opts = {
        border = 'rounded'
      },
      preview_window = false,
      title = true,
      mouse_providers = { 'LSP' },
      mouse_delay = 1000,
    })
    
    -- Mapeo
    vim.keymap.set("n", "K", require("hover").hover, {desc = "hover.nvim"})
    vim.keymap.set("n", "<leader>gk", require("hover").hover_select, {desc = "hover.nvim (select)"})
  end
}
}
