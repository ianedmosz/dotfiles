return {
{
    "mason-org/mason-lspconfig.nvim",
    opts = {
        ensure_installed = { "marksman" },  -- <-- THIS installs the Marksman LSP
        automatic_enable = false,           -- <-- because you already handle LSP setup manually
    },
    dependencies = {
        { "mason-org/mason.nvim", opts = {} },
        "neovim/nvim-lspconfig",
    },
}
}
