local notify = vim.notify
vim.notify = function(msg, ...)
  if msg:match("lspconfig.*deprecated") or msg:match("position_encoding param is required") then
    return
  end
  notify(msg, ...)
end

local on_attach = require("nvchad.configs.lspconfig").on_attach
local capabilities = require("nvchad.configs.lspconfig").capabilities

-- ============= NVIM-CMP SETUP =============

-- TODO: Implement error handling for invalid input

local cmp = require('cmp')
local lspkind = require('lspkind')
local luasnip = require('luasnip')
require("luasnip.loaders.from_vscode").lazy_load()

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp', priority = 1000 },
    { name = 'luasnip', priority = 750 },
    { name = 'buffer', priority = 500 },
    { name = 'path', priority = 250 },
  }),
  formatting = {
    format = lspkind.cmp_format({
      mode = 'symbol_text', -- Opciones: 'text', 'text_symbol', 'symbol_text', 'symbol'
      maxwidth = 50,
      ellipsis_char = '...',
      before = function(entry, vim_item)
        -- Agregar el nombre de la fuente
        vim_item.menu = ({
          nvim_lsp = "[LSP]",
          luasnip = "[Snip]",
          buffer = "[Buf]",
          path = "[Path]",
        })[entry.source.name]
        return vim_item
      end
    })
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  experimental = {
    ghost_text = true,
  },
})

capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)


-- ============= NAVIC SETUP =============
local navic = require("nvim-navic")

navic.setup({
 icons = {
        File          = "󰈙 ",
        Module        = " ",
        Namespace     = "󰌗 ",
        Package       = " ",
        Class         = "󰌗 ",
        Method        = "󰆧 ",
        Property      = " ",
        Field         = " ",
        Constructor   = " ",
        Enum          = "󰕘",
        Interface     = "󰕘",
        Function      = "󰊕 ",
        Variable      = "󰆧 ",
        Constant      = "󰏿 ",
        String        = "󰀬 ",
        Number        = "󰎠 ",
        Boolean       = "◩ ",
        Array         = "󰅪 ",
        Object        = "󰅩 ",
        Key           = "󰌋 ",
        Null          = "󰟢 ",
        EnumMember    = " ",
        Struct        = "󰌗 ",
        Event         = " ",
        Operator      = "󰆕 ",
        TypeParameter = "󰊄 ",
    },
  lsp = {
    auto_attach = true,
    preference = { "clangd", "pyright", "rust_analyzer", "jdtls" },
  },
  highlight = true,
  separator = " > ",
  depth_limit = 0,
  depth_limit_indicator = "..",
  safe_output = true,
  lazy_update_context = false,
  click = false,
})


vim.api.nvim_create_autocmd({"CursorMoved", "CursorMovedI", "BufWinEnter"}, {
  callback = function()
    if navic.is_available() then
      vim.wo.winbar = " " .. navic.get_location()
    else
      vim.wo.winbar = ""
    end
  end,
})

-- ============= LSP CONFIG =============
local lspconfig = require("lspconfig")

local function enhanced_hover()
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients == 0 then
    return
  end
  
  local client = clients[1]
  local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
  
  vim.lsp.buf_request(0, 'textDocument/hover', params, function(err, result, ctx, config)
    if err or not result or not result.contents then
      return
    end
    
    local markdown_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
    markdown_lines = vim.lsp.util.trim_empty_lines(markdown_lines)
    
    if vim.tbl_isempty(markdown_lines) then
      return
    end
    
    vim.lsp.util.open_floating_preview(markdown_lines, 'markdown', {
      border = 'rounded',
      max_width = 80,
      max_height = 30,
      focusable = true,
      focus = false,
      close_events = { "CursorMoved", "BufLeave", "InsertEnter", "FocusLost" },
    })
  end)
end

local custom_on_attach = function(client, bufnr)
  on_attach(client, bufnr)
  
  -- ✨ SEMANTIC HIGHLIGHTING ✨
  if client.server_capabilities.semanticTokensProvider then
    vim.lsp.semantic_tokens.start(bufnr, client.id)
  end
  
  -- Navic
  if client.server_capabilities.documentSymbolProvider then
    navic.attach(client, bufnr)
  end
  
  -- Keymaps mejorados
  local opts = { buffer = bufnr, noremap = true, silent = true }
  
  vim.keymap.set('n', 'K', enhanced_hover, vim.tbl_extend('force', opts, { desc = "Info detallada" }))
  vim.keymap.set('n', 'gK', vim.lsp.buf.hover, vim.tbl_extend('force', opts, { desc = "Hover normal" }))
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, vim.tbl_extend('force', opts, { desc = "Ir a definición" }))
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, vim.tbl_extend('force', opts, { desc = "Ir a declaración" }))
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, vim.tbl_extend('force', opts, { desc = "Ir a implementación" }))
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, vim.tbl_extend('force', opts, { desc = "Ver referencias" }))
  vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, vim.tbl_extend('force', opts, { desc = "Code actions" }))
  vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, vim.tbl_extend('force', opts, { desc = "Renombrar" }))
  vim.keymap.set('n', '<leader>ds', vim.lsp.buf.document_symbol, vim.tbl_extend('force', opts, { desc = "Símbolos del documento" }))
  vim.keymap.set('n', '<leader>ws', vim.lsp.buf.workspace_symbol, vim.tbl_extend('force', opts, { desc = "Símbolos del workspace" }))
  vim.keymap.set('n', '<leader>f', function() vim.lsp.buf.format({ async = true }) end, vim.tbl_extend('force', opts, { desc = "Formatear" }))
end

-- ============= CLANGD (C/C++) =============
lspconfig.clangd.setup({
  on_attach = custom_on_attach,
  capabilities = capabilities,
  cmd = {
    "clangd",
    "--background-index",
    "--clang-tidy",                    -- Análisis estático
    "--header-insertion=iwyu",
    "--completion-style=detailed",
    "--function-arg-placeholders",
    "--fallback-style=llvm",
    "--all-scopes-completion",
    "--pretty",
    "--pch-storage=memory",
    "-j=4",
    "--header-insertion-decorators",
    "--enable-config",                 -- Permite .clangd config
    "--offset-encoding=utf-16",        -- Mejor compatibilidad
    "--log=error",                     -- Menos ruido en logs
  },
  root_dir = function(fname)
    local util = require("lspconfig.util")
    return util.root_pattern(
      '.clangd',
      '.clang-tidy',
      '.clang-format',
      'compile_commands.json',
      'compile_flags.txt',
      'configure.ac',
      '.git'
    )(fname) or util.path.dirname(fname)
  end,
  single_file_support = true,
  init_options = {
    clangdFileStatus = true,
    usePlaceholders = true,
    completeUnimported = true,
    semanticHighlighting = true,
  },
})

-- ============= PYTHON (Pyright) =============
lspconfig.pyright.setup({
  on_attach = custom_on_attach,
  capabilities = capabilities,
  settings = {
    python = {
      analysis = {
        typeCheckingMode = "basic",        -- off, basic, strict
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
        diagnosticMode = "workspace",      -- openFilesOnly o workspace
        autoImportCompletions = true,
        -- Análisis más detallado
        diagnosticSeverityOverrides = {
          reportUnusedImport = "warning",
          reportUnusedVariable = "warning",
          reportDuplicateImport = "warning",
          reportUnusedFunction = "information",
          reportMissingImports = "error",
        },
      },
    },
  },
})

-- ============= RUST (rust-analyzer) =============
lspconfig.rust_analyzer.setup({
  on_attach = custom_on_attach,
  capabilities = capabilities,
  settings = {
    ['rust-analyzer'] = {
      checkOnSave = {
        command = "clippy",              -- Usa clippy para mejores análisis
      },
      cargo = {
        allFeatures = true,
        loadOutDirsFromCheck = true,
      },
      procMacro = {
        enable = true,
      },
      diagnostics = {
        enable = true,
        experimental = {
          enable = true,
        },
      },
      inlayHints = {
        enable = true,
        chainingHints = true,
        parameterHints = true,
        typeHints = true,
      },
    },
  },
})

-- ============= JAVA (JDTLS) =============
vim.api.nvim_create_autocmd("FileType", {
  pattern = "java",
  callback = function()
    local jdtls = require("jdtls")
    
    -- Buscar workspace
    local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ':p:h:t')
    local workspace_dir = vim.fn.stdpath('data') .. '/jdtls-workspace/' .. project_name
    
    local config = {
      cmd = {
        'jdtls',
        '-data', workspace_dir,
      },
      root_dir = vim.fs.dirname(vim.fs.find({
        '.git', 
        'mvnw', 
        'gradlew', 
        'pom.xml', 
        'build.gradle'
      }, { upward = true })[1]),
      on_attach = custom_on_attach,
      capabilities = capabilities,
      settings = {
        java = {
          eclipse = {
            downloadSources = true,
          },
          configuration = {
            updateBuildConfiguration = "interactive",
          },
          maven = {
            downloadSources = true,
          },
          implementationsCodeLens = {
            enabled = true,
          },
          referencesCodeLens = {
            enabled = true,
          },
          references = {
            includeDecompiledSources = true,
          },
          format = {
            enabled = true,
          },
          -- Análisis más estricto
          errors = {
            incompleteClasspath = {
              severity = "warning",
            },
          },
        },
        signatureHelp = { enabled = true },
        completion = {
          favoriteStaticMembers = {
            "org.junit.jupiter.api.Assertions.*",
            "org.mockito.Mockito.*",
          },
        },
        sources = {
          organizeImports = {
            starThreshold = 9999,
            staticStarThreshold = 9999,
          },
        },
      },
      init_options = {
        bundles = {},
      },
    }
    jdtls.start_or_attach(config)
  end,
})

-- ============= HTML/CSS =============
local servers = { "html", "cssls" }
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup({
    on_attach = custom_on_attach,
    capabilities = capabilities,
  })
end

-- ============= DIAGNÓSTICOS MEJORADOS =============
vim.o.updatetime = 300  -- Más rápido (era 1000)

vim.diagnostic.config({
  virtual_text = {
    severity = { min = vim.diagnostic.severity.WARN },  -- Solo warnings y errors
    source = "if_many",
    prefix = "●",
  },
  float = { 
    border = "rounded",
    source = "always",
    header = "",
    prefix = "",
    focusable = true,
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = true,
})

-- Símbolos de diagnóstico personalizados
local signs = { Error = "󰅚 ", Warn = "󰀪 ", Hint = "󰌶 ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- Mostrar diagnósticos en hover
vim.api.nvim_create_autocmd("CursorHold", {
  callback = function()
    vim.diagnostic.open_float(nil, { 
      focus = false, 
      scope = "cursor",
      border = "rounded",
    })
  end,
})

-- ============= INLAY HINTS =============
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.server_capabilities.inlayHintProvider then
      vim.lsp.inlay_hint.enable(true, { bufnr = args.buf })
    end
  end,
})

-- ============= SEMANTIC TOKENS PRIORITY =============
vim.highlight.priorities.semantic_tokens = 125

-- Comando para inspeccionar highlights
vim.api.nvim_create_user_command('InspectHighlight', function()
  local result = vim.treesitter.get_captures_at_cursor(0)
  print("Treesitter captures: " .. vim.inspect(result))
  
  local buf = vim.api.nvim_get_current_buf()
  local row, col = unpack(vim.api.nvim_win_get_cursor(0))
  local token = vim.lsp.semantic_tokens.get_at_pos(buf, row - 1, col)
  print("Semantic token: " .. vim.inspect(token))
end, {})

-- Toggle inlay hints
vim.keymap.set('n', '<leader>th', function()
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
end, { desc = "Toggle Inlay Hints" })
