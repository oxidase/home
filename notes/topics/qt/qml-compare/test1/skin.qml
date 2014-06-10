import QtQuick 2.0

Rectangle {
    objectName: "test"
    width: 1200
    height: 480
    color: "#000080"
    opacity: 0.4
    property bool check: false
    property string zzz: 'hello'
    Text {
        id: text1
        objectName: "test_name"
        text: qsTr("Skin 1")
        anchors.centerIn: parent
    }

    states: [
        State {
            name: 'init'
            when: !check
            PropertyChanges { target: text1; text: 'init skin1AA' }
        },
        State {
            name: 'done'
            when: check
            PropertyChanges { target: text1; text: 'init skin1AA' }
        }
        ]

    transitions: Transition {
        SequentialAnimation {
            PropertyAnimation { property: "x"; duration: 2000 }
            ColorAnimation { duration: 1000 }
        }
    }

    property int xxx: 222
    property int yyy: 222
    property int xxxx: 222
    property int yyyy: 222
    property variant o: [1,2,3,4,5]
    property int yyyyy: 222
}
