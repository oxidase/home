import QtQuick 2.0
import "qml"
import Helpers 1.0

Rectangle {
    width: 1200
    height: 1000

    state: 'skin'

    DiffHelper{
        id: helper
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

    onStateChanged: console.log(state)

    MouseArea {
        anchors.fill: parent
        onClicked: {
            Qt.quit();
        }
    }
}
