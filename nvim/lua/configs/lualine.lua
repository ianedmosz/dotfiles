ocal lualine = require("lualine")

lualine.setup({
  options = {
    theme = "auto",
    globalstatus = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = {
      "filename",
      "lsp_progress",
      {
        function()
          local ts = require("nvim-treesitter.parsers")
          local cur = ts.get_parser(0)
          if not cur then
            return ""
          end
          local lang = cur:lang()
          return " " .. lang
        end,
        cond = function()
          return package.loaded["nvim-treesitter"]
        end,
      },
    },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
})

