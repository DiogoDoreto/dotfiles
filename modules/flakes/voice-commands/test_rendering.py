"""Render the real overlay in a fresh process without a working EGL context."""

from pathlib import Path
import resource
import sys
from unittest.mock import patch

from PySide6.QtTest import QTest
from PySide6.QtQuick import QQuickWindow
from PySide6.QtWidgets import QApplication


def main():
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    sys.path.insert(0, str(Path(sys.argv[1]).parent))
    from grid import GridController

    app = QApplication([])
    app.setQuitOnLastWindowClosed(False)
    controller = GridController(app, "unused")
    # Exercise the production controller/QML with an offscreen window in the sandbox.
    with patch.object(app, "platformName", return_value="wayland"):
        controller.command("click", controller.generation)
    QTest.qWait(200)
    assert isinstance(controller.window, QQuickWindow)
    image = controller.window.grabWindow()
    assert not image.isNull(), "Overlay did not render a frame"
    center = image.pixelColor(image.width() // 2, image.height() // 2)
    assert center.name() == "#ff4040", f"Target crosshair was not rendered: {center.name()}"
    controller.cleanup()
    print("Overlay frame and crosshair rendered without EGL")


if __name__ == "__main__":
    main()
