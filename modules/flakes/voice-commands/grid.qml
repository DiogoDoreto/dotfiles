import QtQuick
import QtQuick.Window
import org.kde.layershell as LayerShell

Window {
    id: overlay
    visible: false
    color: "transparent"
    width: targetScreen.geometry.width
    height: targetScreen.geometry.height
    flags: Qt.FramelessWindowHint | Qt.WindowDoesNotAcceptFocus | Qt.WindowTransparentForInput
    LayerShell.Window.layer: LayerShell.Window.LayerOverlay
    LayerShell.Window.keyboardInteractivity: LayerShell.Window.KeyboardInteractivityNone
    LayerShell.Window.exclusionZone: -1
    LayerShell.Window.screen: targetScreen

    property var region: ({x: 0, y: 0, width: width, height: height})
    property string breadcrumb: "Full screen"
    property bool dryRun: true

    // Shade only the area outside the selected rectangle.
    Repeater {
        model: [
            {x: 0, y: 0, w: overlay.width, h: region.y},
            {x: 0, y: region.y + region.height, w: overlay.width,
             h: overlay.height - region.y - region.height},
            {x: 0, y: region.y, w: region.x, h: region.height},
            {x: region.x + region.width, y: region.y,
             w: overlay.width - region.x - region.width, h: region.height}
        ]
        Rectangle {
            required property var modelData
            x: modelData.x; y: modelData.y
            width: modelData.w; height: modelData.h
            color: "#55000000"
        }
    }

    Item {
        x: region.x; y: region.y
        width: region.width; height: region.height
        Rectangle { anchors.fill: parent; color: "transparent"; border.color: "#ffdf60" }
        Repeater {
            model: 9
            Item {
                required property int index
                x: (index % 3) * region.width / 3
                y: Math.floor(index / 3) * region.height / 3
                width: region.width / 3; height: region.height / 3
                Rectangle { anchors.fill: parent; color: "transparent"; border.color: "#aaffdf60" }
                Text {
                    anchors.centerIn: parent
                    visible: region.width >= 150 && region.height >= 120
                    text: index + 1; color: "white"
                    font.pixelSize: 24; font.bold: true
                    style: Text.Outline; styleColor: "black"
                }
            }
        }
    }

    // A persistent target marker, including when the cell becomes smaller than its labels.
    Item {
        x: region.x + region.width / 2
        y: region.y + region.height / 2
        Rectangle { x: -12; y: -1; width: 25; height: 3; color: "#ff4040" }
        Rectangle { x: -1; y: -12; width: 3; height: 25; color: "#ff4040" }
    }

    Rectangle {
        // Keep the legend away from the target, including targets along screen edges.
        x: region.x + region.width / 2 < overlay.width / 2 ? overlay.width - width - 12 : 12
        y: region.y + region.height / 2 < overlay.height / 2 ? overlay.height - height - 12 : 12
        width: legend.width + 24; height: legend.height + 24
        color: "#e6222222"; radius: 8
        Column {
            id: legend
            x: 12; y: 12; spacing: 8
            Text { text: "Mouse grid" + (dryRun ? " — preview only" : ""); color: "#ffdf60"; font.pixelSize: 18 }
            Text { text: breadcrumb; color: "white"; font.pixelSize: 16 }
            // Enlarged reference keypad, rather than unreadable numbers at deep levels.
            Text {
                visible: region.width < 150 || region.height < 120
                text: "1   2   3\n4   5   6\n7   8   9"
                color: "white"; font.family: "monospace"; font.pixelSize: 22
            }
            Text { text: "1–9: narrow · back · reset\nclick: finish · cancel: exit"; color: "white"; font.pixelSize: 14 }
        }
    }
}
