#This file belongs in $HOME/.config/qtile/ and it should be called 'config.py'

import os
import subprocess

from typing import List  # noqa: F401

from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
# terminal = guess_terminal()

keys = [
    Key([mod], "Tab", lazy.layout.next(),
        desc="Switch window focus to other pane(s) of stack"),

    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "m", lazy.layout.shrink()),
    Key([mod], "n", lazy.layout.normalize()),
    Key([mod], "o", lazy.layout.maximize()),
    Key([mod, "shift"], "space", lazy.layout.flip()),
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),

    Key([mod], "p", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod], "Return", lazy.spawn("alacritty"), desc="Launch terminal"),
    Key([mod], "s", lazy.spawn("firefox"), desc="Launch firefox"),
    Key([mod, "shift"], "s", lazy.spawn("chromium"), desc="Launch chromium"),
    Key([mod], "f", lazy.spawn("pcmanfm"), desc="Launch pcmanfm"),
    Key([mod], "e", lazy.spawn("emacs"), desc="Launch emacs"),
   
    Key([mod], "c", lazy.window.kill(), desc="Kill focused window"),
    Key([mod], "q", lazy.restart(), desc="Restart qtile"),
    Key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown qtile"),

]


groups = [Group("1", label="1. WWW"),
          Group("2", label="2. PS1"),
          Group("3", label="3. VIM"),
          Group("4", label="4. EMC"),
          Group("5", label="5. DOC"),
          Group("6", label="6. { }"),
          Group("7", label="7. >>>"),
          Group("8", label="8. PCM"),
          Group("9", label="9. SYS")]

for i in groups:
        keys.extend([
            # mod1 + letter of group = switch to group
            Key([mod], i.name, lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name)),

            # mod1 + shift + letter of group = switch to & move focused window to group
            Key([mod, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name)),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
                                                                                ])


layouts = [
    layout.Max(),
    # layout.Floating(),
    # layout.Stack(num_stacks=2),
    # Try more layouts by unleashing below layouts.
    # layout.Bsp(),
    # layout.Columns(),
    # layout.Matrix(),
    layout.MonadTall(border_focus = '#e2ff00', border_normal = '#007777', margin = 8),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='ubuntu mono',
    fontsize=16,
    padding=1,

)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                #widget.CurrentLayout(),
                widget.Image(filename = '/home/alex/Pictures/python.png', margin = 4),
                widget.GroupBox(inactive = '#9fb7d6', active = '#c79cff', disable_drag = True, highlight_method = 'line', highlight_color = ['#1e1f29', '#1e1f29'], other_screen_border = '#33323f', this_current_screen_border = '#ff0000'),
                widget.Prompt(),
                widget.WindowName(foreground = '#bdbdbd'),
                widget.Chord(chords_colors={'launch': ("#ff0000", "#323232"),}, name_transform=lambda name: name.upper(),),
                #widget.TextBox("default config", name="default"),
                #widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                widget.Systray(),
                widget.Clock(format='%a %d-%m-%y %H:%M ', foreground = '#7ffc5b'),
                widget.CurrentLayoutIcon(scale = 0.7, foreground = '#000000'),

                #widget.QuickExit(),
            ],
            30, background="#1e1f29"
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = True 
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])



auto_fullscreen = False 
focus_on_window_activation = "smart"

wmname = "LG3D"

#@hook.subscribe.startup
#def startup():

#    home = os.path.expanduser('~/.config/qtile/startup.sh')
#    subprocess.call([home])
