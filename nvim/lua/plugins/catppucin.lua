return {
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "mocha",
        background = {
          light = "latte",
          dark = "mocha",
        },
        transparent_background = false,
        show_end_of_buffer = false,
        term_colors = true,
        dim_inactive = {
          enabled = false,
          shade = "dark",
          percentage = 0.15,
        },
        no_italic = false,
        no_bold = false,
        no_underline = false,
        styles = {
          comments = { "italic" },
          conditionals = { "italic" },
          loops = {},
          functions = {},
          keywords = {},
          strings = {},
          variables = {},
          numbers = {},
          booleans = {},
          properties = {},
          types = {},
          operators = {},
        },
        color_overrides = {},
        custom_highlights = function(colors)
          return {
            -- ✨ SEMANTIC TOKENS HIGHLIGHTS ✨
            ["@lsp.type.variable"] = { fg = colors.text },
            ["@lsp.type.variable.c"] = { fg = colors.blue },
            ["@lsp.type.variable.cpp"] = { fg = colors.blue },
            ["@lsp.type.parameter"] = { fg = colors.maroon, style = { "italic" } },
            ["@lsp.type.parameter.c"] = { fg = colors.maroon, style = { "italic" } },
            ["@lsp.type.parameter.cpp"] = { fg = colors.maroon, style = { "italic" } },
            ["@lsp.type.property"] = { fg = colors.teal },
            ["@lsp.type.function"] = { fg = colors.blue, style = { "bold" } },
            ["@lsp.type.method"] = { fg = colors.blue, style = { "bold" } },
            ["@lsp.type.class"] = { fg = colors.yellow },
            ["@lsp.type.struct"] = { fg = colors.yellow },
            ["@lsp.type.enum"] = { fg = colors.yellow },
            ["@lsp.type.type"] = { fg = colors.yellow },
            ["@lsp.type.interface"] = { fg = colors.yellow },
            ["@lsp.type.namespace"] = { fg = colors.peach },
            ["@lsp.type.typeParameter"] = { fg = colors.maroon },
            ["@lsp.type.macro"] = { fg = colors.mauve },
            ["@lsp.type.enumMember"] = { fg = colors.teal },
            ["@lsp.type.concept"] = { fg = colors.yellow },
            
            -- Modifiers
            ["@lsp.mod.readonly"] = { style = { "italic" } },
            ["@lsp.mod.static"] = { style = { "bold" } },
            ["@lsp.mod.deprecated"] = { style = { "strikethrough" } },
            ["@lsp.mod.global"] = { fg = colors.red },
          }
        end,
        integrations = {
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          treesitter = true,
          notify = true,
          mini = {
            enabled = true,
            indentscope_color = "",
          },
          native_lsp = {
            enabled = true,
            virtual_text = {
              errors = { "italic" },
              hints = { "italic" },
              warnings = { "italic" },
              information = { "italic" },
            },
            underlines = {
              errors = { "underline" },
              hints = { "underline" },
              warnings = { "underline" },
              information = { "underline" },
            },
            inlay_hints = {
              background = true,
            },
          },
          semantic_tokens = true,
        },
      })
      
      -- Aplicar el colorscheme
      vim.cmd.colorscheme("catppuccin")
    end,
  },
}
