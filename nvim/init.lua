vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46/"
vim.g.mapleader = " "
vim.g.deprecation_warnings = false
vim.opt.cursorline = true
vim.o.sessionoptions = "buffers,curdir,folds,help,tabpages,winsize,winpos,localoptions"
-- FORCE ENABLE NETRW - Add this section here!
vim.g.loaded_netrw = 0 -- Set to 0 to ensure it loads (nil or 0 both work)
vim.g.loaded_netrwPlugin = 0
-- Optional netrw settings
vim.g.netrw_banner = 0 -- Hide banner
vim.g.netrw_liststyle = 3 -- Tree view
vim.g.netrw_browse_split = 0 -- Open in same window
vim.g.netrw_winsize = 25 -- Window size
-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  {
    "NvChad/NvChad",
    lazy = false,
    branch = "v2.5",
    import = "nvchad.plugins",
  },

  { import = "plugins" },
}, lazy_config)

-- load theme
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require "options"
require "autocmds"
require "custom.init"

vim.schedule(function()
  require "mappings"
  require "configs.netrw"
end)

-- Add netrw keybindings
vim.keymap.set("n", "<leader>E", ":edit .<CR>", { desc = "Open netrw" })
vim.keymap.set("n", "<leader>e", ":Vexplore<CR>", { desc = "Open netrw split" })

require("configs.indent")
vim.keymap.set("n", "<leader>fi", "gg=G``", { desc = "Fix indentación" })

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

require("auto-session").setup {
  auto_session_enable_last_session = false, -- no cargar la última sesión automáticamente
  auto_restore_enabled = false,             -- restaurar solo con comando
  auto_save_enabled = true,                 -- guarda sesión al cerrar
  auto_session_root_dir = vim.fn.stdpath("data").."/sessions/",
  suppressed_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
}

require'treesitter-context'.setup{
  enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
  multiwindow = false, -- Enable multiwindow support.
  max_lines = 1, -- How many lines the window should span. Values <= 0 mean no limit.
  min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  line_numbers = true,
  multiline_threshold = 20, -- Maximum number of lines to show for a single context
  trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  mode = 'cursor',  -- Line used to calculate context. Choices: 'cursor', 'topline'
  -- Separator between context and content. Should be a single character string, like '-'.
  -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  separator = nil,
  zindex = 20, -- The Z-index of the context window
  on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
}
