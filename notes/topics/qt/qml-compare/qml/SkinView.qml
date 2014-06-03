import QtQuick 2.0


Item {
    property string pathSkinA: '../test1/transitions.qml'
    property string pathSkinB: '../test2/transitions.qml'

    anchors.fill: parent

    Row {
        anchors.fill: parent
        ListView {
            id: changesView
            height: parent.height
            width: parent.width - 800
            model: null
            delegate: Text{ text: index }
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
                z: 100
                clip: true
                objectName: "skinA"
                source: pathSkinA
                onLoaded: {
                    changesView.model = helper.getChanges(skinA.item, skinB.item)
                }
            }
            Text {
                text: pathSkinB
                font.pixelSize: 20
            }
            Loader {
                id: skinB
                width: 800
                height: 480
                z: 100
                clip: true
                objectName: "skinB"
                source: pathSkinB
            }
        }
    }
}
