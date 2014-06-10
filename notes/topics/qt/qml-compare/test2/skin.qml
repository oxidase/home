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
            name: 'done'
            when: check
            PropertyChanges { target: text1; text: 'init skin2' }
        },
        State {
            name: 'init'
            when: !check
            PropertyChanges { target: text1; text: 'init skin1AAA' }
        }
        // State {
        //     name: 'init2'
        //     when: check
        //     changes: [
        //     PropertyChanges { target: text1; text: 'init skin2' }
        //     ]
        // }
        ]

    transitions: Transition {
        SequentialAnimation {
            PropertyAnimation { property: "x"; duration: 2000 }
            ColorAnimation { duration: 1000 }
        }
    }

    property int zzz: 333
    property int yyy: 222
    property int xxxx: 333
    property int yyyy: 333
    property variant o: 42
    property int yyyyy: 333
}
