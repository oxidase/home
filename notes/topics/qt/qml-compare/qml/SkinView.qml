import QtQuick 2.0


Item {
    anchors.fill: parent

    property bool skinsLoaded: loaderSkinA.status === Loader.Ready && loaderSkinB.status === Loader.Ready
    onSkinsLoadedChanged: if (skinsLoaded) helper.getChanges(loaderSkinA.item, loaderSkinB.item)

    function touchCurrentIndex() {
        // quick workaround to update rectangles
        changesView.currentIndexChanged()
    }

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
        clip: true
        highlightMoveVelocity: -1
        flickDeceleration: 1000
        boundsBehavior: Flickable.StopAtBounds
        delegate: Rectangle {
            property string  state: ListView.view.state
            property variant modelData: model
            width: ListView.view.width
            height: childrenRect.height
            color: ListView.isCurrentItem ? 'lightgray' : 'white'
            Text { id: whatText; text: what; font.bold: true }
            Text { id: pathText; text: path; anchors.top: whatText.bottom }
            Text { id: valuesTextM; anchors.top: pathText.bottom; text: changesView.model === helper.deletions ? oldValue
                                                                      : changesView.model === helper.insertions ? newValue
                                                                      : (oldValue.toString() ? oldValue : "''") + ' → ' + (newValue.toString() ? newValue : "''") }
            MouseArea {
                anchors.fill: parent
                onPressed: changesView.currentIndex = index
            }
        }
    }

    ScrollIndicator {
        id: scroller
        visible: changesView.visible
        anchors.right: changesView.right
        anchors.top: changesView.top
        anchors.bottom: changesView.bottom
        position: changesView.visibleArea.yPosition
        zoom: changesView.visibleArea.heightRatio
        shown: changesView.moving
        knob_height: Math.max(50, changesView.height * Math.min(1.0, changesView.height / changesView.contentHeight))
        onDragPosition: changesView.contentY = position * (changesView.contentHeight - changesView.height)
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
            onTextChanged: console.log("!!!!!", text)
        }
        Loader {
            id: loaderSkinA
            width: 800
            height: 480
            clip: true
            objectName: 'skinA'
            // source: textSkinA.text
            sourceComponent: Qt.createComponent(textSkinA.text)
            asynchronous: true
            Text {
                width: parent.width
                anchors.verticalCenter: parent.verticalCenter
                visible: parent.status === Loader.Error
                text: 'Error while loading component A:\n' + parent.sourceComponent.errorString()
                font.pixelSize: 20
                wrapMode: Text.WordWrap
            }
            onSourceComponentChanged: console.log('A sourceComponent', sourceComponent)
            onStatusChanged: console.log('A status', status)
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
            // source: textSkinB.text
            sourceComponent: Qt.createComponent(textSkinB.text)
            asynchronous: true
            Text {
                width: parent.width
                anchors.verticalCenter: parent.verticalCenter
                visible: parent.status === Loader.Error
                text: 'Error while loading component B:\n' + parent.sourceComponent.errorString()
                font.pixelSize: 20
                wrapMode: Text.WordWrap
            }
            onSourceComponentChanged: console.log('B sourceComponent', sourceComponent)
            onStatusChanged: console.log('B status', status)
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
