return{
  -- En tu plugins.lua
{
  "mfussenegger/nvim-dap",
  dependencies = {
    "rcarriga/nvim-dap-ui",
    "nvim-neotest/nvim-nio",
    "theHamsta/nvim-dap-virtual-text",  -- Muestra valores inline
  },
  keys = {
    { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle Breakpoint" },
    { "<leader>dc", function() require("dap").continue() end, desc = "Continue" },
    { "<leader>di", function() require("dap").step_into() end, desc = "Step Into" },
    { "<leader>do", function() require("dap").step_over() end, desc = "Step Over" },
    { "<leader>dO", function() require("dap").step_out() end, desc = "Step Out" },
    { "<leader>dt", function() require("dapui").toggle() end, desc = "Toggle DAP UI" },
  },
  config = function()
    local dap = require("dap")
    local dapui = require("dapui")
    
    dapui.setup()
    require("nvim-dap-virtual-text").setup()
    
    -- Auto abrir/cerrar UI
    dap.listeners.after.event_initialized["dapui_config"] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated["dapui_config"] = function()
      dapui.close()
    end
    
    -- C/C++/Rust (necesitas instalar codelldb desde Mason)
    dap.adapters.codelldb = {
      type = 'server',
      port = "${port}",
      executable = {
        command = vim.fn.stdpath("data") .. "/mason/bin/codelldb",
        args = {"--port", "${port}"},
      }
    }
    
    dap.configurations.cpp = {
      {
        name = "Launch file",
        type = "codelldb",
        request = "launch",
        program = function()
          return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
      },
    }
    dap.configurations.c = dap.configurations.cpp
    
    -- Python
    dap.adapters.python = {
      type = 'executable',
      command = 'python3',
      args = { '-m', 'debugpy.adapter' },
    }
    dap.configurations.python = {
      {
        type = 'python',
        request = 'launch',
        name = "Launch file",
        program = "${file}",
      },
    }
  end,
}
}
