import QtQuick 2.0

Rectangle {
    objectName: "test"
    width: 1200
    height: 480
    color: "#008000"
    opacity: 0.5
    property bool check: true

    Text {
        id: text1
        text: qsTr("Skin 1")
        anchors.centerIn: parent
    }

    states: [
        State {
            name: 'init2'
            when: check
            changes: [
            PropertyChanges { target: text1; text: 'init skin2' }
            ]
        }
    ]
}
