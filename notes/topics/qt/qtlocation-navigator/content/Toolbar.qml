import QtQuick 2.1

Item {
    id: toolbar
    anchors {left: parent.left;  right: parent.right; bottom: parent.bottom}
    height: 0
    opacity: 0

    function show() {
        state = 'VISIBLE'
    }

    function hide() {
        state = ''
    }

    MouseArea {
        anchors.fill: parent
        hoverEnabled: true
        onExited: hide()
    }

    Rectangle {
        id: toolbarRect
        anchors {left: parent.left; right: parent.right; bottom: parent.bottom}
        height: 80
        color: "white"

        Row {
            anchors.fill: parent
            Rectangle {
                height: parent.height; width: height;
                color: "gray"; border.color: "black";
                Text { anchors.centerIn: parent;  text: "Clear" }
                MouseArea { z: 100; anchors.fill: parent; onClicked: mapPage.clear() }
            }
            Rectangle {
                height: parent.height; width: height;
                color: "gray"; border.color: "black";
                Text { anchors.centerIn: parent;  text: "Route" }
                MouseArea { z: 100; anchors.fill: parent; onClicked: mapPage.route() }
            }
        }
    }

    

    states: [
         State {
             name: 'VISIBLE'
             PropertyChanges { target: toolbar; visible: true; opacity: .7; height: 100 }
         }
    ]

    transitions: [
         Transition {
             to: 'VISIBLE'
             NumberAnimation { target: toolbar; property: "height"; from: 0; to: toolbarRect.height; duration: 100}
         },
         Transition {
             to: ''
             SequentialAnimation {
                 NumberAnimation { target: toolbar; property: "opacity"; duration: 500}
                 NumberAnimation { target: toolbar; property: "height"; to: 0; duration: 100}
             }
         }
    ]
}
