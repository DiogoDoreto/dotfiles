"""Reusable help presentation; resolved usage comes from shared feature policy."""

from html import escape

from PySide6.QtWidgets import QTextBrowser, QVBoxLayout, QWidget


class HelpWindow:
    def __init__(self, sections):
        self.window = None
        self.sections = sections

    def show(self):
        if self.window is None:
            self.window = QWidget()
            self.window.setWindowTitle("Voice Commands — Help")
            self.window.resize(640, 560)
            layout = QVBoxLayout(self.window)
            text = QTextBrowser(self.window)
            text.setHtml("<h1>Voice Commands</h1>" + "".join(
                f"<h2>{escape(title)}</h2><p>{escape(body)}</p>" for title, body in self.sections))
            layout.addWidget(text)
        self.window.showNormal()
        self.window.raise_()
        self.window.activateWindow()

    def cleanup(self):
        if self.window is not None:
            self.window.close()
            self.window.deleteLater()
            self.window = None
