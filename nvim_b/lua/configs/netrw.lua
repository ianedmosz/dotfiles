-- lua/custom/netrw.lua
-- Force load netrw completely

vim.g.loaded_netrw = nil
vim.g.loaded_netrwPlugin = nil

-- Source the plugin file
local netrw_plugin = vim.fn.expand('$VIMRUNTIME/plugin/netrwPlugin.vim')
if vim.fn.filereadable(netrw_plugin) == 1 then
  vim.cmd('source ' .. netrw_plugin)
end

-- CRITICAL: Force load the autoload file
local netrw_autoload = vim.fn.expand('$VIMRUNTIME/autoload/netrw.vim')
if vim.fn.filereadable(netrw_autoload) == 1 then
  vim.cmd('source ' .. netrw_autoload)
end

-- Configure netrw
vim.g.netrw_banner = 1
vim.g.netrw_liststyle = 1
vim.g.netrw_browse_split = 0
vim.g.netrw_winsize = 25

-- Disable treesitter for netrw buffers to avoid conflicts
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'netrw',
  callback = function()
    vim.treesitter.stop()
  end,
})

-- Create commands that use netrw
vim.api.nvim_create_user_command('Ex', function(opts)
  local path = opts.args ~= '' and opts.args or '.'
  vim.fn['netrw#Explore'](0, 0, 0, path)
end, { nargs = '?', complete = 'dir' })

vim.api.nvim_create_user_command('Vex', function()
  vim.cmd('vsplit')
  vim.fn['netrw#Explore'](0, 0, 0, '.')
end, {})

vim.api.nvim_create_user_command('Sex', function()
  vim.cmd('split')
  vim.fn['netrw#Explore'](0, 0, 0, '.')
end, {})

-- Keybindings

vim.keymap.set('n', '<leader>cd', function()
  vim.fn['netrw#Explore'](0, 0, 0, '.')
end, { desc = 'Open netrw (cd)' })

-- AUTO-OPEN netrw when opening a directory
vim.api.nvim_create_autocmd('VimEnter', {
  pattern = '*',
  callback = function()
    vim.schedule(function()
      local args = vim.fn.argv()
      if #args > 0 then
        local arg = args[1]
        if type(arg) == 'string' and arg ~= '' and vim.fn.isdirectory(arg) == 1 then
          -- Close any default buffer
          if vim.fn.bufname('%') == '' and vim.fn.line('$') == 1 and vim.fn.getline(1) == '' then
            vim.cmd('enew')
          end
          vim.fn['netrw#Explore'](0, 0, 0, arg)
        end
      end
    end)
  end,
})
