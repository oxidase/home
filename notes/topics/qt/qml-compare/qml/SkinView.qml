import QtQuick 2.0


Item {
    property string pathSkinA: '../test1/skin.qml'
    property string pathSkinB: '../test2/skin.qml'

    anchors.fill: parent

    Row {
        anchors.fill: parent
        ListView {
            height: parent.height
            width: parent.width - 800
            model: 5
            delegate: Text{ text: modelData}
        }

        Column {
            height: parent.height
            width: 800
            Text {
                text: pathSkinA
                font.pixelSize: 20
            }
            Loader {
                id: skinA
                width: 800
                height: 480
                clip: true
                objectName: "skinA"
                source: pathSkinA
            }
            Text {
                text: pathSkinB
                font.pixelSize: 20
            }
            Loader {
                id: skinB
                width: 800
                height: 480
                clip: true
                objectName: "skinB"
                source: pathSkinB
            }
        }
    }
}
