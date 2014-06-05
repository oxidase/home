import QtQuick 2.0
import Qt.labs.settings 1.0
import Helpers 1.0
import "qml"


Rectangle {
    id: main
    width: 1400
    height: 1020

    Settings {
        property alias state: main.state
        property alias directoryA: directoryView.directoryA
        property alias directoryB: directoryView.directoryB
        property alias selectedFileName: directoryView.selectedFileName
    }

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

    states: [
    State {
        name: 'directory'
    },
    State {
        name: 'skin'
    }
    ]
}
