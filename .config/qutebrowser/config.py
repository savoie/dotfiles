import os
import signal
import yaml

import qutebrowser.app

from qutebrowser.utils import objreg
from qutebrowser.misc.crashsignal import SignalHandler

with open(os.path.expanduser('~/.cache/wal/colors.yml'), 'r') as yml_file:
    colors = yaml.load(yml_file)

c.aliases = {**{
    'q': 'quit',
    'w': 'session-save',
    'wq': 'quit --save',
    'x': 'quit --save',
    'b': 'buffer',
    'bp': 'tab-prev',
    'bn': 'tab-next',
    'bd': 'tab-close'
}, **{'b'+str(i): 'buffer '+str(i) for i in range(1, 10)}}

c.fonts.monospace = 'Fantasque Sans Mono'

# completion
c.completion.show = 'auto'

# statusbar
c.statusbar.hide = True
c.statusbar.padding = {'top': 5, 'bottom': 5, 'left': 5, 'right': 5}

# hints
c.hints.border = '5px solid ' + colors['special']['foreground']
c.fonts.hints = '10pt monospace'
c.hints.chars = 'asdghjkl'  # home row excluding f
config.bind('f', 'leave-mode', mode='hint')

# tabs
c.tabs.last_close = 'blank'
c.tabs.show = 'switching'
c.tabs.indicator.width = 0
c.tabs.favicons.show = 'pinned'
c.tabs.title.alignment = 'center'
c.tabs.padding = {'top': 5, 'bottom': 5, 'left': 5, 'right': 5}

# colours
color_mapping = {
    colors['special']['foreground']: [
        'colors.statusbar.normal.fg',
        'colors.statusbar.command.fg',
        'colors.statusbar.url.fg',
        'colors.statusbar.url.success.http.fg',
        'colors.hints.bg',
        'colors.tabs.odd.fg',
        'colors.tabs.even.fg',
        'colors.tabs.selected.odd.bg',
        'colors.tabs.selected.even.bg'
    ],
    colors['special']['background']: [
        'colors.statusbar.normal.bg',
        'colors.statusbar.command.bg',
        'colors.hints.fg',
        'colors.tabs.bar.bg',
        'colors.tabs.odd.bg',
        'colors.tabs.even.bg',
        'colors.tabs.selected.odd.fg',
        'colors.tabs.selected.even.fg'
    ],
    colors['colors']['color1']: [
        'colors.statusbar.url.success.https.fg',
        'colors.hints.match.fg'
    ],
    colors['colors']['color3']: [
        'colors.statusbar.url.hover.fg'
    ]
}
for color, setting in [(color, setting) for color, settings in color_mapping.items()
                       for setting in settings]:
    config.set(setting, color)

# reload config on SIGUSR1
# this not at all supported, expect to break on update
class PatchedSignalHandler(SignalHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        def sigusr1_handler(signum, _frame):
            config_commands = objreg.get('config-commands')
            config_commands.config_source()
        self._orig_handlers[signal.SIGUSR1] = signal.signal(signal.SIGUSR1, sigusr1_handler)
qutebrowser.app.crashsignal.SignalHandler = PatchedSignalHandler
