return {
  "rmagatti/auto-session",
  lazy = false,
  opts = {
    suppressed_dirs = { "~/", "~/Projects", "~/Downloads", "/" },
    log_level = 'info',

    -- Desactiva carga automática de última sesión
    auto_session_enable_last_session = false,

    -- Habilita restauración solo con comando
    auto_restore_enabled = true,

    -- Guarda sesiones automáticamente
    auto_save_enabled = true,

    -- Carpeta donde se guardan las sesiones
    auto_session_root_dir = vim.fn.stdpath("data").."/sessions/",
  },
}
