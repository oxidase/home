import QtQuick 2.0

Rectangle {
    id: rect
    width: 100; height: 100
    color: "red"

    property int zzz: 111

    MouseArea { id: mouseArea; anchors.fill: parent }

    states: State {
        name: "brighter"
        when: mouseArea.pressed
        PropertyChanges { target: rect; color: "yellow"; x: 50 }
    }
    transitions: Transition {
        SequentialAnimation {
            PropertyAnimation { property: "x"; duration: 1000 }
            ColorAnimation { duration: 1000 }
        }
    }
        Component.onCompleted: console.log('completed')
}
