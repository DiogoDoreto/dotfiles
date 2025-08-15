#!/usr/bin/env python3

# from: https://discuss.kde.org/t/a-script-to-toggle-tablet-mode-or-touch-mode-on-plasma/19224

import subprocess
import gi

gi.require_version('Gio', '2.0')
from gi.repository import Gio, GLib

KDE_VERSION = 6
OBJECT_PATH = '/kwinrc'
INTERFACE_NAME = 'org.kde.kconfig.notify'
SIGNAL_NAME = 'ConfigChanged'

current_mode: str = subprocess.check_output([f"kreadconfig{KDE_VERSION}", "--file", "kwinrc", "--group", "Input", "--key", "TabletMode", "--default", "auto"]).decode(encoding='utf-8').strip()
if current_mode == "on":
    subprocess.check_call([f"kwriteconfig{KDE_VERSION}", "--file", "kwinrc", "--group", "Input", "--key", "TabletMode", "off"])
else:
    subprocess.check_call([f"kwriteconfig{KDE_VERSION}", "--file", "kwinrc", "--group", "Input", "--key", "TabletMode", "on"])

connection = Gio.bus_get_sync(Gio.BusType.SESSION, None)
Gio.DBusConnection.emit_signal(connection, None, OBJECT_PATH, INTERFACE_NAME, SIGNAL_NAME, GLib.Variant.new_tuple(GLib.Variant('a{saay}', {'Input': [b'TabletMode']})))
