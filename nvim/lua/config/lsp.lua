-- ~/.config/nvim/lua/config/lsp.lua

local notify = vim.notify
vim.notify = function(msg, ...)
  if msg:match("lspconfig.*deprecated") or msg:match("position_encoding param is required") then
    return
  end
  notify(msg, ...)
end

-- ============= NVIM-CMP SETUP =============
local cmp = require("cmp")
local lspkind = require("lspkind")
local luasnip = require("luasnip")
require("luasnip.loaders.from_vscode").lazy_load()

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp", priority = 1000 },
    { name = "luasnip", priority = 750 },
    { name = "buffer", priority = 500 },
    { name = "path", priority = 250 },
  }),
  formatting = {
    format = lspkind.cmp_format({
      mode = "symbol_text",
      maxwidth = 50,
      ellipsis_char = "...",
      before = function(entry, vim_item)
        vim_item.menu = ({
          nvim_lsp = "[LSP]",
          luasnip = "[Snip]",
          buffer = "[Buf]",
          path = "[Path]",
        })[entry.source.name]
        return vim_item
      end,
    }),
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  experimental = {
    ghost_text = true,
  },
})

local capabilities
local ok_cmp_lsp, cmp_lsp = pcall(require, "cmp_nvim_lsp")
if ok_cmp_lsp then
  capabilities = cmp_lsp.default_capabilities()
else
  capabilities = vim.lsp.protocol.make_client_capabilities()
end

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
  highlight = false,
  separator = " > ",
  depth_limit = 0,
  depth_limit_indicator = "..",
  safe_output = true,
  lazy_update_context = false,
  click = false,
})

vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufWinEnter" }, {
  callback = function()
    if navic.is_available() then
      vim.wo.winbar = " " .. navic.get_location()
    else
      vim.wo.winbar = ""
    end
  end,
})

-- ============= LSP CONFIG BASE =============

local function enhanced_hover()
  local clients = vim.lsp.get_clients({ bufnr = 0 })
  if #clients == 0 then
    return
  end

  local client = clients[1]
  local params = vim.lsp.util.make_position_params(0, client.offset_encoding)

  vim.lsp.buf_request(0, "textDocument/hover", params, function(err, result, ctx, config)
    if err or not result or not result.contents then
      return
    end

    local markdown_lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
    markdown_lines = vim.lsp.util.trim_empty_lines(markdown_lines)

    if vim.tbl_isempty(markdown_lines) then
      return
    end

    vim.lsp.util.open_floating_preview(markdown_lines, "markdown", {
      border = "rounded",
      max_width = 50,
      max_height = 20,
      focusable = true,
      focus = false,
      close_events = { "CursorMoved", "BufLeave", "InsertEnter", "FocusLost" },
    })
  end)
end

local custom_on_attach = function(client, bufnr)
  if client.server_capabilities.documentSymbolProvider then
    navic.attach(client, bufnr)
  end

  if client.server_capabilities.semanticTokensProvider then
    client.server_capabilities.semanticTokensProvider = nil
  end

  if client.server_capabilities.documentHighlightProvider then
    client.server_capabilities.documentHighlightProvider = false
  end

  local opts = { buffer = bufnr, noremap = true, silent = true }

  vim.keymap.set("n", "K", enhanced_hover, opts)
  vim.keymap.set("n", "gK", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
  vim.keymap.set("n", "<leader>ds", vim.lsp.buf.document_symbol, opts)
  vim.keymap.set("n", "<leader>ws", vim.lsp.buf.workspace_symbol, opts)
  vim.keymap.set("n", "<leader>f", function()
    vim.lsp.buf.format({ async = true })
  end, opts)
end

-- ============= JAVA (JDTLS) =============
vim.api.nvim_create_autocmd("FileType", {
  pattern = "java",
  callback = function()
    local jdtls = require("jdtls")

    local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
    local workspace_dir = vim.fn.stdpath("data") .. "/jdtls-workspace/" .. project_name

    local root_files = vim.fs.find({
      ".git",
      "mvnw",
      "gradlew",
      "pom.xml",
      "build.gradle",
    }, { upward = true })

    local root_dir = root_files[1] and vim.fs.dirname(root_files[1]) or vim.fn.getcwd()

    local config = {
      cmd = {
        "jdtls",
        "-data",
        workspace_dir,
      },
      root_dir = root_dir,
      on_attach = custom_on_attach,
      capabilities = capabilities,
      settings = {
        java = {
          eclipse = { downloadSources = true },
          configuration = { updateBuildConfiguration = "interactive" },
          maven = { downloadSources = true },
          implementationsCodeLens = { enabled = true },
          referencesCodeLens = { enabled = true },
          references = { includeDecompiledSources = true },
          format = { enabled = true },
          errors = {
            incompleteClasspath = { severity = "warning" },
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


-- ======== clangd ==================
vim.lsp.config("clangd", {
  cmd = { "clangd" },
  on_attach = custom_on_attach,
  capabilities = capabilities,
  filetypes = { "c", "cpp", "objc", "objcpp" },

  -- nuevo estilo: root_markers, no usamos lspconfig.util
  root_markers = {
    { "compile_commands.json", ".clangd" },
    ".git",
  },

  settings = {
    clangd = {
      inlayHints = {
        enabled = true,
        presentStyle = "Block",
      },
    },
  },
})

-- ======== pyright ==================
vim.lsp.config("pyright", {
  cmd = { "pyright-langserver", "--stdio" },
  on_attach = custom_on_attach,
  capabilities = capabilities,
  filetypes = { "python" },
  root_markers = {
    "pyrightconfig.json",
    "setup.py",
    "__init__.py",
    ".git",
  },
  settings = {
    python = {
      analysis = {
        autoSearchPaths = true,
        typeCheckingMode = "basic",
      },
    },
  },
})

-- ======== marksman ==================
vim.lsp.config("marksman", {
  cmd = { "marksman", "server" },
  on_attach = custom_on_attach,
  capabilities = capabilities,
  filetypes = { "markdown" },
  root_markers = { ".git", "README.md" },
})

-- ======== lua_ls ==================
vim.lsp.config("lua_ls", {
  on_attach = custom_on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
    },
  },
})

-- ======== rust_analyzer ==================
vim.lsp.config("rust_analyzer", {
  on_attach = custom_on_attach,
  capabilities = capabilities,
})


-- ======== ts_ls (nuevo tsserver) ==================
vim.lsp.config("ts_ls", {
  on_attach = custom_on_attach,
  capabilities = capabilities,
})

-- habilita todos los servers configurados arriba
vim.lsp.enable({
  "clangd",
  "pyright",
  "marksman",
  "lua_ls",
  "rust_analyzer",
  "ts_ls",
})
-- ============= DIAGNÓSTICOS MEJORADOS =============
vim.o.updatetime = 300

vim.diagnostic.config({
  virtual_text = {
    severity = { min = vim.diagnostic.severity.WARN },
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

local signs = { Error = "󰅚 ", Warn = "󰀪 ", Hint = "󰌶 ", Info = " " }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

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

