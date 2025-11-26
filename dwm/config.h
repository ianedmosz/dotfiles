#include <X11/XF86keysym.h>

#define FORCE_VSPLIT 1

static unsigned int borderpx        = 2;        /* border pixel of windows */
static unsigned int snap            = 32;       /* snap pixel */
static const unsigned int gappih    = 20;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 20;       /* vert inner gap between windows */
static const unsigned int gappoh    = 20;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 30;       /* vert outer gap between windows and screen edge */
static       int smartgaps          = 1;        /* 1 means no outer gap when there is only one window */
static const int swallowfloating    = 0;        /* 1 means swallow floating windows by default */
static int showbar                  = 1;        /* 0 means no bar */
static const int showtitle          = 1;        /* 0 means no title */
static const int showtags           = 1;        /* 0 means no tags */
static const int showlayout         = 1;        /* 0 means no layout indicator */
static const int showstatus         = 1;        /* 0 means no status bar */
static const int showfloating       = 0;        /* 0 means no floating indicator */
static int topbar                   = 1;        /* 0 means bottom bar */

static const char *fonts[] = {
    "JetBrainsMono Nerd Font Mono:style=Bold:size=16",
};

static const char dmenufont[]       = "JetBrainsMono Nerd Font Mono:style=Bold:size=16";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#005577";


/* TokyoNight colors */
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

static const char *colors[][3] = {
    /*               fg       bg       border */
    [SchemeNorm] = { col_fg,  col_bg,   col_brblk },
    [SchemeSel]  = { col_red, col_bg,   col_mag   },
};

/* tagging */
static const char *tags[] = { "一", "二", "三", "四", "五", "六", "七", "八", "九" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            1,           -1 },
	{ "Firefox",  NULL,       NULL,       0,            0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

#include "vanitygaps.c"


static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "󰝘",        tile },    /* first entry is default */
	{ "",        NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
 	{ "",        spiral },
 	{ "[\\]",     dwindle },
};

/* key definitions */
#define MODKEY Mod1Mask
#define TAGKEYS(KEY,TAG)                                                                                               \
       &((Keychord){1, {{MODKEY, KEY}},                                        view,           {.ui = 1 << TAG} }), \
       &((Keychord){1, {{MODKEY|ControlMask, KEY}},                            toggleview,     {.ui = 1 << TAG} }), \
       &((Keychord){1, {{MODKEY|ShiftMask, KEY}},                              tag,            {.ui = 1 << TAG} }), \
       &((Keychord){1, {{MODKEY|ControlMask|ShiftMask, KEY}},                  toggletag,      {.ui = 1 << TAG} }),

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf", col_gray4, NULL };
static const char *termcmd[]  = { "alacritty", NULL };
static const char *rofi[]  = { "rofi", "-show", "drun", "-theme", "~/.config/rofi/config.rasi", NULL };


static Keychord *keychords[] = {
    /* key count, modifier/key sequence,            function,        argument */

    &((Keychord){1, {{MODKEY, XK_p}},               spawn,          {.v = dmenucmd } }),
    &((Keychord){1, {{MODKEY, XK_Return}},          spawn,          {.v = termcmd } }),
    &((Keychord){1, {{MODKEY, XK_d}},               spawn,          {.v = rofi } }),

    &((Keychord){1, {{MODKEY, XK_b}},               togglebar,      {0} }),
    &((Keychord){1, {{MODKEY, XK_j}},               focusstack,     {.i = +1 } }),
    &((Keychord){1, {{MODKEY, XK_k}},               focusstack,     {.i = -1 } }),
    &((Keychord){1, {{MODKEY, XK_i}},               incnmaster,     {.i = +1 } }),
    &((Keychord){1, {{MODKEY, XK_p}},               incnmaster,     {.i = -1 } }),
    &((Keychord){1, {{MODKEY, XK_g}},               setmfact,       {.f = -0.05} }),
    &((Keychord){1, {{MODKEY, XK_h}},               setmfact,       {.f = +0.05} }),

    &((Keychord){1, {{MODKEY, XK_z}},               incrgaps,       {.i = +3 } }),
    &((Keychord){1, {{MODKEY, XK_x}},               incrgaps,       {.i = -3 } }),
    &((Keychord){1, {{MODKEY, XK_a}},               togglegaps,     {0} }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_a}},     defaultgaps,    {0} }),

    &((Keychord){1, {{MODKEY, XK_Tab}},             view,           {0} }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_q}},               killclient,     {0} }),

    &((Keychord){1, {{MODKEY|ShiftMask, XK_t}},     setlayout,      {.v = &layouts[0]} }),
    // &((Keychord){1, {{MODKEY, XK_f}},               setlayout,      {.v = &layouts[1]} }),
    &((Keychord){1, {{MODKEY, XK_m}},               setlayout,      {.v = &layouts[2]} }),
    &((Keychord){1, {{MODKEY, XK_c}},               setlayout,      {.v = &layouts[3]} }),
    &((Keychord){1, {{MODKEY, XK_o}},               setlayout,      {.v = &layouts[4]} }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_Return}},setlayout,      {0} }),

    &((Keychord){1, {{MODKEY|ShiftMask, XK_f}},     fullscreen,     {0} }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_space}}, togglefloating, {0} }),

    &((Keychord){1, {{MODKEY, XK_0}},               view,           {.ui = ~0 } }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_0}},     tag,            {.ui = ~0 } }),

    &((Keychord){1, {{MODKEY, XK_comma}},           focusmon,       {.i = -1 } }),
    &((Keychord){1, {{MODKEY, XK_period}},          focusmon,       {.i = +1 } }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_comma}}, tagmon,         {.i = -1 } }),
    &((Keychord){1, {{MODKEY|ShiftMask, XK_period}},tagmon,         {.i = +1 } }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_f}}, spawn, {.v = (const char*[]){"firefox", NULL}} }),

    // Keychords for navigating to tags (small hands/emacs pinky)
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_1}}, view, {.ui = 1 << 0} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_2}}, view, {.ui = 1 << 1} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_3}}, view, {.ui = 1 << 2} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_4}}, view, {.ui = 1 << 3} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_5}}, view, {.ui = 1 << 4} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_6}}, view, {.ui = 1 << 5} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_7}}, view, {.ui = 1 << 6} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_8}}, view, {.ui = 1 << 7} }),
    &((Keychord){2, {{MODKEY, XK_space}, {0, XK_9}}, view, {.ui = 1 << 8} }),

    
    // Emacs Scripts
    // &((Keychord){2, {{MODKEY, XK_e}, {0, XK_t}}, spawn, SHCMD("$HOME/scripts/tmux-dmenu.sh")}),
    // &((Keychord){2, {{MODKEY, XK_e}, {0, XK_a}}, spawn, SHCMD("$HOME/scripts/tmux-dmenu.sh")}),
    // &((Keychord){2, {{MODKEY, XK_e}, {0, XK_t}}, spawn, SHCMD("$HOME/scripts/tmux-dmenu.sh")}),

    // TAGKEYS
    TAGKEYS(                        XK_1,                      0)
    TAGKEYS(                        XK_2,                      1)
    TAGKEYS(                        XK_3,                      2)
    TAGKEYS(                        XK_4,                      3)
    TAGKEYS(                        XK_5,                      4)
    TAGKEYS(                        XK_6,                      5)
    TAGKEYS(                        XK_7,                      6)
    TAGKEYS(                        XK_8,                      7)
    TAGKEYS(                        XK_9,                      8)

    &((Keychord){1, {{MODKEY|ShiftMask,   XK_c}},   quit,           {0} }),
    &((Keychord){1, {{MODKEY|ControlMask, XK_r}},   quit,           {1} }),

    &((Keychord){1, {{0, XF86XK_AudioRaiseVolume}}, spawn, {.v = (const char*[]){"pactl", "set-sink-volume", "@DEFAULT_SINK@", "+3%", NULL} } }),
    &((Keychord){1, {{0, XF86XK_AudioLowerVolume}}, spawn, {.v = (const char*[]){"pactl", "set-sink-volume", "@DEFAULT_SINK@", "-3%", NULL} } }),
    &((Keychord){1, {{0, XF86XK_MonBrightnessUp}},   spawn, {.v = (const char*[]){"brightnessctl", "set", "+5%", NULL} } }),
    &((Keychord){1, {{0, XF86XK_MonBrightnessDown}}, spawn, {.v = (const char*[]){"brightnessctl", "set", "5%-", NULL} } }),
};


/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
