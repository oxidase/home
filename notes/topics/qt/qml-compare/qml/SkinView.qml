import QtQuick 2.0


Item {
    anchors.fill: parent

    property bool skinsLoaded: loaderSkinA.status === Loader.Ready && loaderSkinB.status === Loader.Ready
    onSkinsLoadedChanged: if (skinsLoaded) helper.getChanges(loaderSkinA.item, loaderSkinB.item)

    Row {
        id: tabButtons
        height: 40
        width: parent.width - skinsColumn.width - 15
        TabButton {
            id: fileViewButton
            height: parent.height
            width: 40
            icon.source: 'assets/List_icon.svg'
            onPressed: main.state = 'directory'
        }
        TabButton {
            height: parent.height
            width: (parent.width - fileViewButton.width) / 3
            label.text: 'Deleted'
            selected: changesView.model === helper.deletions
            onPressed: changesView.model = helper.deletions
        }
        TabButton {
            height: parent.height
            width: (parent.width - fileViewButton.width) / 3
            label.text: 'Modified'
            selected: changesView.model === helper.modifications
            onPressed: changesView.model = helper.modifications
        }
        TabButton {
            height: parent.height
            width: (parent.width - fileViewButton.width) / 3
            label.text: 'Inserted'
            selected: changesView.model === helper.insertions
            onPressed: changesView.model = helper.insertions
        }
    }

    ListView {
        id: changesView
        anchors.top: tabButtons.bottom
        anchors.bottom: parent.bottom
        anchors.left: tabButtons.left
        anchors.right: tabButtons.right
        model: helper.modifications
        currentIndex: 0
        delegate: Rectangle {
            property variant modelData: model
            width: ListView.view.width
            height: childrenRect.height
            color: ListView.isCurrentItem ? 'lightgray' : 'white'
            Text { id: whatText; text: what; font.bold: true }
            Text { id: pathText; text: path; anchors.top: whatText.bottom }
            Text { id: valuesText; text:  (oldValue.toString() ? oldValue : "''") + ' → ' + (newValue.toString() ? newValue : "''"); anchors.top: pathText.bottom }
            MouseArea {
                anchors.fill: parent
                onPressed: changesView.currentIndex = index
            }

            Component.onCompleted: console.log(newItem.mapToItem(null, 0, 0).x, newItem.mapToItem(null, 0, 0).y, newItem.width, newItem.height)
        }
    }

    Column {
        id: skinsColumn
        anchors.left: tabButtons.right
        width: 800
        height: parent.height
        Text {
            id: textSkinA
            text: directoryView.directoryA + '/' + directoryView.selectedFileName
            font.pixelSize: 20
        }
        Loader {
            id: loaderSkinA
            width: 800
            height: 480
            clip: true
            objectName: 'skinA'
            source: textSkinA.text
            asynchronous: true
        }
        Text {
            id: textSkinB
            text: directoryView.directoryB + '/' + directoryView.selectedFileName
            font.pixelSize: 20
        }
        Loader {
            id: loaderSkinB
            width: 800
            height: 480
            clip: true
            objectName: 'skinB'
            source: textSkinB.text
            asynchronous: true
        }
    }

    Rectangle {
        property var item: changesView.currentIndex >= 0 && changesView.currentItem !== null && changesView.currentItem.modelData !== null ? changesView.currentItem.modelData.oldItem : null
        property var xy: item !== null ? item.mapToItem(null, item.x*0, item.y*0) : null
        visible: item !== null
        color: "transparent"
        border.color: "blue"
        border.width: 3
        x: xy !== null ? xy.x : 0
        y: xy !== null ? xy.y : 0
        z: 10000
        width: item !== null ? item.width : 0
        height: item !== null ? item.height : 0
    }

    Rectangle {
        property var item: changesView.currentIndex >= 0 && changesView.currentItem !== null && changesView.currentItem.modelData !== null ? changesView.currentItem.modelData.newItem : null
        property var xy: item !== null ? item.mapToItem(null, item.x*0, item.y*0) : null
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

    Keys.onPressed: {
        console.log("move left", changesView.currentIndex);
        if (event.key == Qt.Key_Left) {
            directoryView.nextModifiedFile(-1)
            changesView.currentIndex = 0
            event.accepted = true;
        } else if (event.key == Qt.Key_Right) {
            directoryView.nextModifiedFile(+1)
            changesView.currentIndex = 0
            event.accepted = true;
        } else if (event.key == Qt.Key_Up) {
            if (changesView.count > 0) {
                var index = (changesView.currentIndex - 1)  % changesView.count;
                changesView.currentIndex = index >= 0 ? index : index + changesView.count;
            }
            event.accepted = true;
        } else if (event.key == Qt.Key_Down) {
            if (changesView.count > 0) {
                changesView.currentIndex = (changesView.currentIndex + 1)  % changesView.count;
            }
            event.accepted = true;
        }
    }
    focus: true
}
