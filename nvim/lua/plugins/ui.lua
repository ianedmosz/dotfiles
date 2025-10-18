return {
  {
    "NvChad/ui",
    opts = {
      statusline = {
        overriden_modules = function(modules)
          -- añade el contexto de Treesitter a la sección central (lualine_c)
          table.insert(modules, 3, function()
            local ok, ts_context = pcall(require, "treesitter-context")
            if not ok then
              return ""
            end

            local context = ts_context.statusline({
              separator = " > ",
              mode = "cursor",
            })

            if context == "" then
              return ""
            end

            return " " .. context
          end)
        end,
      },
    },
  },
}

