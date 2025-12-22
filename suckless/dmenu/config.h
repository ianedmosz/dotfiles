static int topbar = 1;              /* 0 = bar abajo, 1 = arriba */

static const char *fonts[] = {
    "JetBrainsMono Nerd Font Mono:style=Bold:size=14",
};

static const char *prompt = NULL;   /* texto del prompt, ej: "Run:" o NULL */

/* Colores TokyoNight, igual que en tu dwm */
static const char col_bg[]     = "#1a1b26";  // background
static const char col_fg[]     = "#a9b1d6";  // foreground
static const char col_blk[]    = "#32344a";  // black (normal)
static const char col_red[]    = "#f7768e";  // red
static const char col_grn[]    = "#9ece6a";  // green
static const char col_ylw[]    = "#e0af68";  // yellow
static const char col_blu[]    = "#7aa2f7";  // blue
static const char col_mag[]    = "#ad8ee6";  // magenta
static const char col_cyn[]    = "#0db9d7";  // cyan (highlight)
static const char col_brblk[]  = "#444b6a";  // bright black

static const char *colors[SchemeLast][2] = {
    /*          fg       bg      */
    [SchemeNorm] = { col_fg, col_bg  },  /* texto normal */
    [SchemeSel]  = { col_red, col_blk }, /* texto + fondo item seleccionado */
    [SchemeOut]  = { col_bg, col_cyn },  /* casi nunca lo usas, pero lo dejamos lindo */
};

/* número de líneas para modo vertical: 0 = horizontal */
static unsigned int lines      = 0;

/* se considera delimitador para borrar palabras con Ctrl+W */
static const char worddelimiters[] = " ";
