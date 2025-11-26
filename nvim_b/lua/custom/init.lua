-- TODO:  
--
-- FIXME: PLS
--
require("todo-comments").setup {
  signs = true, -- muestra un signo en el gutter
  keywords = { -- puedes personalizar los keywords
    TODO = { icon = "", color = "info" },
    FIXME = { icon = "", color = "error" },
    HACK = { icon = "", color = "warning" },
    NOTE = { icon = "", color = "hint" },
  },
  highlight = {
    multiline = true,   -- resalta TODO en varias líneas si aplica
    keyword = "wide",   -- resalta todo el keyword
    after = "",         -- resalta después del keyword
  },
}
