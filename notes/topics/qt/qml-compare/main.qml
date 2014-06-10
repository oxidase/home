import QtQuick 2.0
import QtQuick.Window 2.1
import Qt.labs.settings 1.0
import Helpers 1.0
import "qml"

Window {
    id: window
    width: 1400
    height: 1020
    title: 'Skins compare tool'

    Settings {
        property alias windowWidth: window.width
        property alias windowHeight: window.height
        property alias windowX: window.x
        property alias windowY: window.y
        property alias state: main.state
        property alias directoryA: directoryView.directoryA
        property alias directoryB: directoryView.directoryB
        property alias selectedFileName: directoryView.selectedFileName
    }

    Rectangle {
        id: main
        anchors.fill: parent
        state: 'skin'

        DiffHelper{
            id: helper
        }

        MouseArea {
            anchors.fill: parent
            onClicked: {
                // Qt.quit();
            }
        }

        DirectoryView {
            id: directoryView
            visible: parent.state == 'directory'
        }

        SkinView {
            id: skinView
            visible: parent.state === 'skin'
        }
    }

    onWidthChanged: skinView.touchCurrentIndex()
    onHeightChanged: skinView.touchCurrentIndex()
}
