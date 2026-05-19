-- Monitors
hl.monitor({ -- Local monitor
    output = "eDP-1",
    mode = "preferred",
    scale = 1
})

--- At home
hl.monitor({ -- Ultrawide
    output = "desc:Samsung Electric Company LS49C95xU HNTX102250",
    position = "-1600x-1440",
    scale = 1
})
hl.monitor({ -- "Desktop" laptop, disabled
    output = "desc:Chimei Innolux Corporation 0x15E7",
    disabled = true
})

--- Fallback
hl.monitor({
    output = "",
    mode = "preferred",
    position = "auto-up",
    scale = 1
})



-- Autostart
hl.on("hyprland.start", function()
    hl.exec_cmd("waybar")
    hl.exec_cmd("discord --start-minimized")
    hl.exec_cmd("slack -u")
    hl.exec_cmd("firefox")
    hl.exec_cmd("LD_PRELOAD=/usr/lib/spotify-adblock.so spotify")
    hl.exec_cmd("kitty bluetui")
end)

-- Variables
hl.config({
    general = {
        gaps_in = 0,
        gaps_out = 0,
        border_size = 1,
        col = {
            active_border = 0xFF00FF00,
            inactive_border = 0xFF000000
        },
    },
    animations = {
        enabled = false,
    },
    dwindle = {
        preserve_split = true,
    },
    master = {
        orientation = "center",
        slave_count_for_center_master = 0,
    },
    misc = {
        disable_hyprland_logo = true,
        middle_click_paste = false,
    },
    input = {
        kb_layout = "eu",
        kb_options = "caps:super",
    },
    cursor = {
        no_hardware_cursors = 1,
    },
})



-- Windowrules
hl.window_rule({
    name = "hide_border_when_only_one_window",
    match = { workspace = "w[t1]" },
    border_size = 0,
})



-- Keybinds
--- Programs
---- Starting
hl.bind("SUPER + Q", hl.dsp.exec_cmd("kitty"))
hl.bind("SUPER + W", hl.dsp.exec_cmd("waybar"))
hl.bind("SUPER + E", hl.dsp.exec_cmd("evince"))
hl.bind("SUPER + R", hl.dsp.exec_cmd("wofi --show drun"))
hl.bind("SUPER + T", hl.dsp.exec_cmd("thunderbird"))
hl.bind("SUPER + I", hl.dsp.exec_cmd("/opt/ida-free-pc*/ida"))
hl.bind("SUPER + A", hl.dsp.exec_cmd("slack"))
hl.bind("SUPER + S", hl.dsp.exec_cmd(
    "LD_PRELOAD=/usr/lib/spotify-adblock.so spotify")
)
hl.bind("SUPER + D", hl.dsp.exec_cmd("discord"))
hl.bind("SUPER + F", hl.dsp.exec_cmd("libreoffice"))
hl.bind("SUPER + G", hl.dsp.exec_cmd("gimp"))
hl.bind("SUPER + B", hl.dsp.exec_cmd("firefox"))
hl.bind("SUPER + N", hl.dsp.exec_cmd("android-studio"))
hl.bind("SUPER + M", hl.dsp.exec_cmd("virtualbox"))
---- Killing
hl.bind("SUPER + ALT + W", hl.dsp.exec_cmd("killall waybar"))
hl.bind("SUPER + ALT + A", hl.dsp.exec_cmd("killall slack"))
hl.bind("SUPER + ALT + S", hl.dsp.exec_cmd("killall spotify"))
hl.bind("SUPER + ALT + D", hl.dsp.exec_cmd("killall -9 Discord"))

--- Misc
---- Screenshots
hl.bind("SUPER + SHIFT + S", hl.dsp.exec_cmd(
    "hyprshot -m region --clipboard-only")
)
hl.bind("SUPER + Print", hl.dsp.exec_cmd("hyprshot -m region"))
---- Locking
hl.bind("SUPER + O", hl.dsp.exec_cmd(
    "killall -9 Discord ; swaylock -f -c 000000")
)
hl.bind("SUPER + SHIFT + O", hl.dsp.exec_cmd(
    "killall -9 Discord ; swaylock -f -c 000000 " ..
    "-i ~/.config/hypr/bsod.png -s stretch -u"
))
hl.bind("SUPER + ALT + SHIFT + O", hl.dsp.exit())

--- Window management
---- General
hl.bind("SUPER + P", hl.dsp.window.pseudo())
hl.bind("SUPER + X", hl.dsp.layout("togglesplit"))
hl.bind("SUPER + C", hl.dsp.window.close())
hl.bind("SUPER + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind("SUPER + SHIFT + ALT + S", hl.dsp.exec_cmd(
    "~/.config/hypr/switch_layout.sh")
)
---- Windows
local vimKeybinds = {
    H = {
        direction = "left",
        resize = {
            x = -30, y = 0,
            xSmall = -1, ySmall = 0
        },
    },
    J = {
        direction = "down",
        resize = {
            x = 0, y = 30,
            xSmall = 0, ySmall = 1
        },
    },
    K = {
        direction = "up",
        resize = {
            x = 0, y = -30,
            xSmall = 0, ySmall = -1
        },
    },
    L = {
        direction = "right",
        resize = {
            x = 30, y = 0,
            xSmall = 1, ySmall = 0
        },
    },
}
for key, cfg in pairs(vimKeybinds) do
    hl.bind("SUPER + " .. key, hl.dsp.focus({ direction = cfg.direction }))
    hl.bind(
        "SUPER + SHIFT + " .. key,
        hl.dsp.window.move({ direction = cfg.direction })
    )
    hl.bind(
        "SUPER + ALT + " .. key,
        hl.dsp.window.resize({
            x = cfg.resize.x,
            y = cfg.resize.y,
            relative = true
        }),
        { repeating = true }
    )
    hl.bind(
        "SUPER + ALT + SHIFT + " .. key,
        hl.dsp.window.resize({
            x = cfg.resize.xSmall,
            y = cfg.resize.ySmall,
            relative = true,
        }),
        { repeating = true }
    )
end
hl.bind("SUPER + mouse:272", hl.dsp.window.drag(),   { mouse = true })
hl.bind("SUPER + mouse:273", hl.dsp.window.resize(), { mouse = true })
---- Workspaces
for i = 1, 10 do
    local key = i % 10
    hl.bind("SUPER + " .. key, hl.dsp.focus({ workspace = i }))
    hl.bind("SUPER + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end
