import QtQuick 2.0
     
Rectangle {
    width: 800
    height: 480
    Text {
        text: qsTr("Hello World")
        anchors.centerIn: parent
    }

    property int index: 0

    ListView {
        anchors.fill: parent
        id: listView
        model: adaptor.models[index]
        delegate: Rectangle {
            width: listView.width
            height: 24
            Text { text: name; font.pixelSize: 20; color: colorRole }
        }
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            // listView.model = adaptor.getModel()
            index = (index + 1) % adaptor.models.length
            // Qt.quit();
        }
    }
}
