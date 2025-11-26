-- ============= INDENTACIÓN POR LENGUAJE =============

local autocmd = vim.api.nvim_create_autocmd

-- C y C++ (4 espacios - estilo Linux/LLVM)
autocmd("FileType", {
  pattern = { "c", "cpp", "h", "hpp" },
  callback = function()
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.expandtab = true
    vim.opt_local.cindent = true  -- Indentación específica para C
    vim.opt_local.cinoptions = "g0,:0,N-s,(0"  -- Opciones de estilo C
  end,
})

-- Python (4 espacios - PEP 8)
autocmd("FileType", {
  pattern = "python",
  callback = function()
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.expandtab = true
    vim.opt_local.textwidth = 88  -- Black formatter default
  end,
})

-- Lua (2 espacios - estilo Neovim)
autocmd("FileType", {
  pattern = "lua",
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
  end,
})

-- Rust (4 espacios - estilo oficial)
autocmd("FileType", {
  pattern = "rust",
  callback = function()
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.expandtab = true
  end,
})

-- Java (4 espacios - estilo Google/Oracle)
autocmd("FileType", {
  pattern = "java",
  callback = function()
    vim.opt_local.tabstop = 4
    vim.opt_local.softtabstop = 4
    vim.opt_local.shiftwidth = 4
    vim.opt_local.expandtab = true
  end,
})

-- Bash/Shell (2 espacios - estilo Google)
autocmd("FileType", {
  pattern = { "sh", "bash", "zsh" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
  end,
})

-- CSS/SCSS/HTML (2 espacios - estilo web estándar)
autocmd("FileType", {
  pattern = { "css", "scss", "sass", "html", "htmldjango" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
  end,
})

-- JSON/YAML (2 espacios)
autocmd("FileType", {
  pattern = { "json", "yaml", "yml" },
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
  end,
})

-- Markdown (2 espacios)
autocmd("FileType", {
  pattern = "markdown",
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
    vim.opt_local.wrap = true
  end,
})
