return {
  "folke/noice.nvim",
  opts = function(_, opts)
    -- Agregamos una regla nueva a las rutas de Noice
    table.insert(opts.routes, {
      filter = {
        event = "lsp",
        kind = "progress",
        cond = function(message)
          local client = vim.tbl_get(message.opts, "progress", "client")
          -- Si el mensaje viene de Java (jdtls), lo saltamos
          return client == "jdtls"
        end,
      },
      opts = { skip = true },
    })
  end,
}
