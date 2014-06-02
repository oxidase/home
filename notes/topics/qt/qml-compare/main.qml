import QtQuick 2.0

Rectangle {
    width: 1200
    height: 1000
    Text {
        text: qsTr("Hello World")
        anchors.centerIn: parent
    }

    Item {
        width: 800
        height: 480
        clip: true
        Loader {
            id: skinA
            objectName: "skinA"
            source: "test/skin1.qml"
        }
    }

    property int index: 0

    MouseArea {
        anchors.fill: parent
        onClicked: {
            Qt.quit();
        }
    }
}
