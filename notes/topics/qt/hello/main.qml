import QtQuick 2.0
     
Rectangle {
    width: 800
    height: 480
    Text {
        text: qsTr("Hello World")
        anchors.centerIn: parent
    }

    ListView {
        anchors.fill: parent
        id: listView
        model: adaptor.getModel()
        delegate: Rectangle {
            width: listView.width
            height: 24
            Text { text: name; font.pixelSize: 20; color: colorRole }
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            listView.model = adaptor.getModel()
            // Qt.quit();
        }
    }
}
