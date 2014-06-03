import QtQuick 2.0

Rectangle {
    width: 200; height: 200

    MouseArea {
        anchors.fill: parent
        onClicked: pageLoader.source = "transitions.qml"
    }

    Loader { id: pageLoader }


}
