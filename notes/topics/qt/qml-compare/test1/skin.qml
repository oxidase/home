import QtQuick 2.0

Rectangle {
    objectName: "test"
    width: 1200
    height: 480
    color: "#000080"
    opacity: 0.4
    property bool check: false
    Text {
        id: text1
        text: qsTr("Skin 1")
        anchors.centerIn: parent
    }

    states: [
        State {
            name: 'init'
            when: !check
            PropertyChanges { target: text1; text: 'init skin1AA' }
        }
    ]
}
