# -*- coding: utf-8 -*-
import os
import re
import socket
import subprocess
from libqtile import qtile
from libqtile.config import Click, Drag, Group, KeyChord, Key, Match, Screen
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from libqtile.lazy import lazy
from typing import List  # noqa: F401

mod = "mod4"
terminal = "alacritty"

keys = [
    # Switch between windows
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    Key([mod], "Tab", lazy.layout.down(), desc="Use the tab key to cycle through windows."),
    #Key([mod], "Tab", lazy.layout.next(), desc="Move window focus to other window"),

    # Move windows
    Key([mod, "control"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "control"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key([mod, "control"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "control"], "k", lazy.layout.shuffle_up(), desc="Move window up"),

    # Resize windows
    Key([mod, "shift"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "shift"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "shift"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "shift"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod], "t", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "b", lazy.hide_show_bar("top")),

    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "c", lazy.window.kill(), desc="Kill focused window"),

    # Applications

    Key([mod], "p", lazy.spawn("application_launcher.sh"), desc="Spawn a command using a prompt widget"),
    Key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
    #Key([mod], "e", lazy.spawn("emacsclient -c"), desc="Launch emacs"),
    Key([mod], "e", lazy.spawn("emacs"), desc="Launch emacs"),
    Key([mod], "f", lazy.spawn("pcmanfm"), desc="Launch a file browser"),
    Key([mod], "s", lazy.spawn("brave"), desc="Launch a web browser"),
    #Key([mod], "g", subprocess.run(["screenshot.sh"])   desc="Take a screenshot."),
    #Key([mod], "b", lazy.spawn("qutebrowser"), desc="Launch a web browser"),

    # Session controls

    Key([mod], "q", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    # Sound
    Key([], "XF86AudioMute", lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -10%")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +10%")),

]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        Key([mod], i.name, lazy.group[i.name].toscreen(), desc="Switch to group {}".format(i.name)),
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name), desc="move focused window to group {}".format(i.name)),
    ])

window_colours = ['#adb1b5', '#51afef', '#000000', '#333333']

colors = [["#232731", "#232731"], # panel background
          ["#3d3f4b", "#434758"], # background for current screen tab
          ["#ffffff", "#ffffff"], # font color for group names
          ["#ff5555", "#ff5555"], # border line color for current tab
          ["#74438f", "#74438f"], # border line color for 'other tabs' and color for 'odd widgets'
          ["#4f76c7", "#4f76c7"], # color for the 'even widgets'
          ["#e1acff", "#e1acff"], # window name
          ["#ecbbfb", "#ecbbfb"],
          ["#4f80c7", "#4f80c7"]] # backbround for inactive screens

layouts = [
    layout.Columns(border_width=2, border_focus=colors[5][0], border_focus_stack=colors[5][0], border_normal=colors[1][0], border_on_single=True, margin=[0, 0, 0, 0]),
    layout.Stack(border_width=0, border_focus=window_colours[2], num_stacks=1, margin=[0, 0, 0, 0]),
]


# prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())

##### DEFAULT WIDGET SETTINGS #####
widget_defaults = dict(font="Jetbrains Mono Nerd Font", fontsize = 12, padding = 0, background=colors[2])

extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                # widget.Sep( linewidth = 0, padding = 6, foreground = colors[2], background = colors[0]),
                # widget.Image( filename = "~/.config/qtile/icons/python-white.png", scale = "False", mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm)}), 
                widget.Sep(linewidth = 0, padding = 6, foreground = colors[4], background = colors[4]),
                widget.CurrentLayoutIcon(background=colors[4], padding=0, scale = 0.6),
                widget.CurrentLayout( foreground = colors[2], background = colors[4], padding=5),
                widget.TextBox( text = '', background=colors[5], foreground = colors[4], padding=0, fontsize=30),
                widget.Sep(linewidth = 0, padding = 6, foreground = colors[5], background = colors[5]),
                widget.GroupBox(
                    disable_drag=True,
                    hide_unused=True,
                    margin_y = 3,
                    margin_x = 0,
                    padding_y = 5,
                    padding_x = 3,
                    borderwidth = 3,
                    active = colors[0],
                    inactive = colors[1],
                    rounded = False,
                    highlight_color = colors[8],
                    highlight_method = "block",
                    this_current_screen_border = colors[8],
                    this_screen_border = colors [4],
                    other_current_screen_border = colors[6],
                    other_screen_border = colors[4],
                    foreground = colors[2],
                    background = colors[5]
                ),
                widget.TextBox( text = '', background=colors[0], foreground = colors[5], padding=0, fontsize=30),
                # widget.Prompt( prompt = prompt, font = "Ubuntu Mono", padding = 10, foreground = colors[3], background = colors[1]),
                widget.Sep( linewidth = 0, padding = 40, foreground = colors[2], background = colors[0]),
                widget.Spacer(background=colors[0]),
                # widget.WindowName( foreground = colors[6], background = colors[0], padding = 0),
                # widget.Systray( background = colors[0], padding = 5),
                # widget.TextBox( text = '', background = colors[0], foreground = colors[4], padding = 0, fontsize = 37),
                # widget.Net( interface = "enp3s0", format = '{down} ↓↑ {up}', foreground = colors[2], background = colors[4], padding = 5),
                # widget.TextBox( text = '', background = colors[4], foreground = colors[5], padding = 0, fontsize = 37),
                # widget.TextBox( text = " 🌡 TEMP NOT SHOWN ", padding = 2, foreground = colors[2], background = colors[5], fontsize = 11),
                # widget.TextBox( text='', background = colors[5], foreground = colors[4], padding = 0, fontsize = 37),
                # widget.TextBox( text = " ⟳", padding = 2, foreground = colors[2], background = colors[4], fontsize = 14),
                # widget.CheckUpdates( update_interval = 1800, distro = "Arch_checkupdates", display_format = "{updates} Updates", foreground = colors[2], mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e sudo pacman -Syu')}, background = colors[4]),
                # widget.TextBox( text = '', background = colors[4], foreground = colors[5], padding = 0, fontsize = 37),
                # widget.TextBox( text = " 🖬", foreground = colors[2], background = colors[5], padding = 0, fontsize = 14),
                # widget.Memory( foreground = colors[2], background = colors[5], mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn(myTerm + ' -e htop')}, padding = 5),
                # widget.TextBox( text='', background = colors[5], foreground = colors[4], padding = 0, fontsize = 37),
                # widget.TextBox( text = " ₿", padding = 0, foreground = colors[2], background = colors[4], fontsize = 12),
                # widget.BitcoinTicker( foreground = colors[2], background = colors[4], padding = 5),
                # widget.TextBox( text = '', background = colors[4], foreground = colors[5], padding = 0, fontsize = 37),
                # widget.TextBox( text = " Vol:", foreground = colors[2], background = colors[5], padding = 0),
                # widget.Volume( foreground = colors[2], background = colors[5], padding = 5),

                # widget.TextBox( text = '', background=colors[0], foreground = colors[4], padding=0, fontsize=30),
                # widget.CurrentLayoutIcon(background=colors[4], padding=0, scale = 0.7),
                widget.CurrentLayout( foreground = colors[2], background = colors[4], padding=5),
                widget.TextBox( text = '', background = colors[4], foreground = colors[5], padding=0, fontsize=30),
                # widget.Sep( linewidth = 0, padding=0, foreground = colors[5], background = colors[5]),
                # widget.Sep(linewidth = 0, padding = 6, foreground = colors[5], background = colors[5]),
                widget.Clock(foreground = colors[2], background = colors[5], format = "%d %B - %H:%M ", padding=0),
            ],
            opacity=1.0,
            size=24,
            background=colors[0]
        )
    )
]

# def window_to_prev_group(qtile):
#     if qtile.currentWindow is not None:
#         i = qtile.groups.index(qtile.currentGroup)
#         qtile.currentWindow.togroup(qtile.groups[i - 1].name)
# 
# def window_to_next_group(qtile):
#     if qtile.currentWindow is not None:
#         i = qtile.groups.index(qtile.currentGroup)
#         qtile.currentWindow.togroup(qtile.groups[i + 1].name)
# 
# def window_to_previous_screen(qtile):
#     i = qtile.screens.index(qtile.current_screen)
#     if i != 0:
#         group = qtile.screens[i - 1].group.name
#         qtile.current_window.togroup(group)
# 
# def window_to_next_screen(qtile):
#     i = qtile.screens.index(qtile.current_screen)
#     if i + 1 != len(qtile.screens):
#         group = qtile.screens[i + 1].group.name
#         qtile.current_window.togroup(group)
# 
# def switch_screens(qtile):
#     i = qtile.screens.index(qtile.current_screen)
#     group = qtile.screens[i - 1].group
#     qtile.current_screen.set_group(group)

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    # default_float_rules include: utility, notification, toolbar, splash, dialog,
    # file_progress, confirm, download and error.
    *layout.Floating.default_float_rules,
    Match(title='Confirmation'),      # tastyworks exit box
    Match(title='Qalculate!'),        # qalculate-gtk
    Match(wm_class='kdenlive'),       # kdenlive
    Match(wm_class='pinentry-gtk-2'), # GPG key password entry
])

auto_fullscreen = True
focus_on_window_activation = "smart"

# @hook.subscribe.startup_once
# def start_once():
#     home = os.path.expanduser('~')
#     subprocess.call([home + '/.config/qtile/autostart.sh'])

wmname = "LG3D"
