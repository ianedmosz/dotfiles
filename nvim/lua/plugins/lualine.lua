return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
  config = function()
    local lualine = require('lualine')

    local colors = {
      bg       = '#1e222a',
      bg_alt   = '#282c34',
      fg       = '#abb2bf',
      yellow   = '#e5c07b',
      cyan     = '#56b6c2',
      darkblue = '#081633',
      green    = '#98c379',
      orange   = '#d19a66',
      violet   = '#c678dd',
      magenta  = '#c678dd',
      blue     = '#61afef',
      red      = '#e86671',
      grey     = '#5c6370',
    }

    local my_lualine_theme = {
      normal = {
        a = { fg = colors.bg, bg = colors.blue, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.bg },
        c = { fg = colors.fg, bg = colors.bg },
      },
      -- Insert Mode (Green)
      insert = {
        a = { fg = colors.bg, bg = colors.green, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.bg },
        c = { fg = colors.fg, bg = colors.bg },
      },
      -- Visual Mode (Violet/Magenta)
      visual = {
        a = { fg = colors.bg, bg = colors.violet, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.bg },
        c = { fg = colors.fg, bg = colors.bg },
      },
      -- Replace Mode (Red)
      replace = {
        a = { fg = colors.bg, bg = colors.red, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.bg },
        c = { fg = colors.fg, bg = colors.bg },
      },
      -- Command/Cmdline Mode (Orange)
      command = {
        a = { fg = colors.bg, bg = colors.orange, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.bg },
        c = { fg = colors.fg, bg = colors.bg },
      },
      -- Terminal Mode (Cyan)
      terminal = {
        a = { fg = colors.bg, bg = colors.cyan, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.bg },
        c = { fg = colors.fg, bg = colors.bg },
      },
      -- Inactive Window (Grey)
      inactive = {
        a = { fg = colors.grey, bg = colors.bg },
        b = { fg = colors.grey, bg = colors.bg },
        c = { fg = colors.grey, bg = colors.bg },
      },
    }

    local conditions = {
      buffer_not_empty = function()
        return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
      end,
      hide_in_width = function()
        return vim.fn.winwidth(0) > 80
      end,
      check_git_workspace = function()
        local filepath = vim.fn.expand('%:p:h')
        local gitdir = vim.fn.finddir('.git', filepath .. ';')
        return gitdir and #gitdir > 0 and #gitdir < #filepath
      end,
    }

    local config = {
      options = {
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = '' },
        theme = my_lualine_theme, -- ⬅️ Custom theme applied here
        disabled_filetypes = {
          statusline = { 'dashboard' },
          winbar = {},
        },
      },
      sections = {
        lualine_a = {'mode'},
        lualine_b = {'FugitiveHead'},
        lualine_y = {},
        lualine_z = {},
        lualine_c = {},
        lualine_x = {},
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_y = {},
        lualine_z = {},
        lualine_c = { 'filename' },
        lualine_x = { 'location' },
      },
    }

    local function ins_left(component)
      table.insert(config.sections.lualine_c, component)
    end

    local function ins_right(component)
      table.insert(config.sections.lualine_x, component)
    end


    -- Nombre de archivo con icono
    ins_left {
      'filename',
      cond = conditions.buffer_not_empty,
      color = { fg = colors.fg, gui = 'bold' },
      symbols = {
        modified = '●',
        readonly = ' ',
        unnamed = ' [Sin nombre]',
      },
      path = 1,

    }

    -- Ubicación en el archivo
    ins_left {
      'location',
      color = { fg = colors.green },
      padding = { left = 0, right = 0 },
    }

    -- Progreso
    ins_left {
      'progress',
      color = { fg = colors.grey },
      padding = { left = 1, right = 1 },
    }

    -- Diagnósticos (MEJORADO - ahora se ven los números)
    ins_left {
      'diagnostics',
      sources = { 'nvim_lsp', 'nvim_diagnostic' },
      sections = { 'error', 'warn', 'info', 'hint' },
      symbols = { 
        error = ' ', 
        warn = ' ', 
        info = ' ',
        hint = '󰌵 '
      },
      diagnostics_color = {
        error = { fg = colors.red },
        warn = { fg = colors.yellow },
        info = { fg = colors.cyan },
        hint = { fg = colors.blue },
      },
      update_in_insert = false,
      always_visible = false,
      padding = { left = 1, right = 1 },
    }

    -- Separador central
    ins_left {
      function()
        return '%='
      end,
    }

    -- LSP activo
    ins_right {
      function()
        local buf_ft = vim.api.nvim_get_option_value('filetype', { buf = 0 })
        local clients = vim.lsp.get_clients()
        if next(clients) == nil then
          return ''
        end
        
        local client_names = {}
        for _, client in ipairs(clients) do
          local filetypes = client.config.filetypes
          if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
            table.insert(client_names, client.name)
          end
        end
        
        if #client_names > 0 then
          return '  ' .. table.concat(client_names, ', ')
        end
        return ''
      end,
      color = { fg = colors.green, gui = 'bold' },
      padding = { left = 1, right = 1 },
    }

    -- Git branch
    ins_right {
      'branch',
      icon = '',
      color = { fg = colors.violet, gui = 'bold' },
      padding = { left = 1, right = 1 },
    }

    -- Git diff
    ins_right {
      'diff',
      symbols = { 
        added = ' ', 
        modified = ' ', 
        removed = ' ' 
      },
      diff_color = {
        added = { fg = colors.green },
        modified = { fg = colors.orange },
        removed = { fg = colors.red },
      },
      cond = conditions.hide_in_width,
      padding = { left = 1, right = 1 },
    }

    -- Tipo de archivo
    ins_right {
      'filetype',
      colored = true,
      icon_only = false,
      color = { fg = colors.fg },
      padding = { left = 1, right = 1 },
    }

    -- Encoding
    ins_right {
      'encoding',
      fmt = string.upper,
      cond = conditions.hide_in_width,
      color = { fg = colors.grey },
      padding = { left = 1, right = 1 },
    }

    -- Formato de archivo (CORREGIDO - con iconos correctos)
    ins_right {
      function()
        local format = vim.bo.fileformat
        local format_icons = {
          unix = '',    -- Linux/Unix
          dos = '',    -- Windows
          mac = '',    -- Mac
        }
        return format_icons[format] or format
      end,
      color = { fg = colors.grey },
      padding = { left = 1, right = 1 },
      separator = '',
    }


    lualine.setup(config)
  end,
}
