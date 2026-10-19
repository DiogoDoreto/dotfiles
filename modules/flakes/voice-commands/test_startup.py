"""Fresh-process probe for desktop Qt plugins loading an older OpenSSL first."""

import ctypes
from pathlib import Path
import runpy
import sys

from PySide6.QtWidgets import QApplication


def main():
    tray, crypto = sys.argv[1:]
    original_init = QApplication.__init__

    def desktop_init(self, *args, **kwargs):
        # Simulate a host platform-theme/style plugin loading crypto during GUI setup.
        # The parent test process already imports QML, so this must run in a fresh one.
        ctypes.CDLL(crypto, ctypes.RTLD_GLOBAL)
        original_init(self, *args, **kwargs)

    QApplication.__init__ = desktop_init
    sys.path.insert(0, str(Path(tray).parent))
    sys.argv = [tray, "--listener", "unused", "--pointer", "unused", "--grid-stdin"]
    runpy.run_path(tray, run_name="__main__")


if __name__ == "__main__":
    main()
