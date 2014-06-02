import QtQuick 2.0


Item {
    property string pathSkinA: '../test1/skin.qml'
    property string pathSkinB: '../test2/skin.qml'

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
                clip: true
                objectName: "skinA"
                source: pathSkinA
                onLoaded: {
                    changesView.model = helper.getChanges(skinA.item, skinB.item)
                    console.log(typeof skinA.item.anchors, typeof skinB.item.anchors, typeof skinA.item)
                    // console.log(skinA.item.anchors, skinB.item.anchors, skinA.item.anchors == skinB.item.anchors)
                    // console.log(skinA.item.anchors.top, skinB.item.anchors.top)
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
                clip: true
                objectName: "skinB"
                source: pathSkinB
            }
        }
    }
}
