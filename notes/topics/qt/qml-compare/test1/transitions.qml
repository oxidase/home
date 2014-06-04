import QtQuick 2.0

Rectangle {
    id: rect
    width: 100; height: 100
    color: "red"
    property int xxx: 222
    property int yyy: 222
    objectName: "test1"

    MouseArea { id: mouseArea; anchors.fill: parent; onClicked: console.log('clicked') }

    states: State {
        name: "brighter"
        when: mouseArea.pressed
        PropertyChanges { target: rect; color: "yellow"; x: 50 }
    }

    transitions: Transition {
        SequentialAnimation {
            PropertyAnimation { property: "x"; duration: 2000 }
            ColorAnimation { duration: 1000 }
        }
    }
    Component.onCompleted: console.log('completed')
}
