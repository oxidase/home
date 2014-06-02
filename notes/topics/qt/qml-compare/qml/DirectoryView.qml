import QtQuick 2.0
import Qt.labs.folderlistmodel 2.1

Item {
    anchors.fill: parent

    property string directoryA: '../test1'
    property string directoryB: '../test2'
    property var contentA: {}
    property var contentB: {}

    function getDirectoryContent(model) {
        var content = {}
        for (var k = 0; k < model.count; ++k) {
            var fileName = model.get(k, 'fileName');
            var filePath = model.get(k, 'filePath');
            var fileMD5 = helper.md5(filePath);
            var fileIsDir = model.get(k, 'fileIsDir');
            console.log(k, fileName, filePath, fileMD5, fileIsDir)
            content[fileName] = {'fileName':fileName, 'filePath':filePath, 'fileMD5':fileMD5, 'fileIsDir':fileIsDir };
        }
        return content
    }

    FolderListModel {
        id: modelA
        folder: directoryA
        nameFilters: ["*.qml"]
        onDataChanged: contentA = getDirectoryContent(this)
        onModelReset: contentA = getDirectoryContent(this)
    }

    FolderListModel {
        id: modelB
        folder: directoryB
        nameFilters: ["*.qml"]
        onDataChanged: contentB = getDirectoryContent(this)
        onModelReset: contentB = getDirectoryContent(this)
    }

    Row {
        anchors.fill: parent

        /// Directory A
        Column {
            width: parent.width / 2
            Text {
                text: directoryA
            }
            ListView {
                width: 200; height: 400

                model: modelA
                delegate: Item {
                    width: childrenRect.width
                    height: childrenRect.height
                    Text {
                        text: fileName
                        color: fileIsDir ? 'black' : (contentB && fileName in contentB) ? (contentB[fileName].fileMD5 === helper.md5(filePath) ? 'gray' : 'black' ) : 'red'
                        font.strikeout: !(fileName in contentB)
                    }
                }
            }
        }

        /// Directory B
        Column {
            width: parent.width / 2
            Text {
                text: directoryB
            }
            ListView {
                width: 200; height: 400

                model: modelB
                delegate: Item {
                    width: childrenRect.width
                    height: childrenRect.height
                    Text {
                        text: fileName
                        color: fileIsDir ? 'black' : (contentA && fileName in contentA) ? (contentA[fileName].fileMD5 === helper.md5(filePath) ? 'gray' : 'black' ) : 'green'
                    }
                }
            }
        }
    }
}