return {
  {
    "saghen/blink.cmp",
    opts = {
      keymap = {
        preset = "none", -- Quitamos los predeterminados para no chocar
        ["<cr>"] = { "accept", "fallback" },
        ["<Tab>"] = { "select_next", "fallback" },
        ["<S-Tab>"] = { "select_prev", "fallback" },
      },
    },
  },
}
