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
            model: helper.modifications
            currentIndex: 0
            delegate: Rectangle {
                property variant modelData: model
                width: ListView.view.width
                height: childrenRect.height
                color: ListView.isCurrentItem ? 'lightgray' : 'white'
                Text { id: whatText; text: what }
                Text { id: pathText; text: path; anchors.top: whatText.bottom }
                Text { id: valuesText; text:  (oldValue ? oldValue : "''") + ' -> ' + (newValue ? newValue : "''"); anchors.top: pathText.bottom }
                Text { text:  oldItem.mapToItem(null, 0, 0).x + ' ' + oldItem.mapToItem(null, 0, 0).y; anchors.top: valuesText.bottom }
                MouseArea {
                    anchors.fill: parent
                    onPressed: changesView.currentIndex = index
                }

                Component.onCompleted: console.log(newItem.mapToItem(null, 0, 0).x, newItem.mapToItem(null, 0, 0).y, newItem.width, newItem.height)
            }
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
                onLoaded: helper.getChanges(skinA.item, skinB.item)
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
                onLoaded: helper.getChanges(skinA.item, skinB.item)
            }
        }
    }

    Rectangle {
        property var item: changesView.currentIndex >= 0 && changesView.currentItem.modelData !== null ? changesView.currentItem.modelData.newItem : null
        property var xy: item !== null ? item.mapToItem(null, 0, 0) : null
        visible: item !== null
        color: "transparent"
        border.color: "green"
        border.width: 3
        x: xy !== null ? xy.x : 0
        y: xy !== null ? xy.y : 0
        z: 10000
        width: item !== null ? item.width : 0
        height: item !== null ? item.height : 0
    }

    Rectangle {
        property var item: changesView.currentIndex >= 0 && changesView.currentItem.modelData !== null ? changesView.currentItem.modelData.oldItem : null
        property var xy: item !== null ? item.mapToItem(null, 0, 0) : null
        visible: item !== null
        color: "transparent"
        border.color: "red"
        border.width: 3
        x: xy !== null ? xy.x : 0
        y: xy !== null ? xy.y : 0
        z: 10000
        width: item !== null ? item.width : 0
        height: item !== null ? item.height : 0
    }

    Component.onCompleted: helper.getChanges(skinA.item, skinB.item)
}
