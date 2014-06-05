import QtQuick 2.0
import QtQuick.Dialogs 1.0
import Qt.labs.folderlistmodel 2.1

Item {
    anchors.fill: parent

    property string directoryA: '../test1'
    property string directoryB: '../test2'
    property string selectedFileName: ''
    property var contentA: {}
    property var contentB: {}
    property var modifiedFiles: []
    property string highlightedName: ''
    property color highlightedColor: 'lightgray'


    onContentAChanged: modifiedFiles = getModifiedFiles()
    onContentBChanged: modifiedFiles = getModifiedFiles()
    function getModifiedFiles() {
        var result = []
        console.log(contentA)
        for (var fileName in contentA) {
            if (fileName in contentB && contentA[fileName].fileMD5 !== contentB[fileName].fileMD5) {
                result.push(fileName);
            }
        }
        if (result.length > 0 && result.indexOf(selectedFileName) === -1) {
            selectedFileName = result[0];
        }
        return result
    }

    function nextModifiedFile(step) {
        if (modifiedFiles.length > 0 && selectedFileName.length > 0) {
            var index = modifiedFiles.indexOf(selectedFileName);
            if (index >= 0) {
                index = (index + step) % modifiedFiles.length;
                if (index < 0) index += modifiedFiles.length;
                selectedFileName = modifiedFiles[index];
            }
        }
    }

    function getDirectoryContent(model) {
        var content = {}
        for (var k = 0; k < model.count; ++k) {
            var fileName = model.get(k, 'fileName');
            var filePath = model.get(k, 'filePath');
            var fileMD5 = helper.md5(filePath);
            var fileIsDir = model.get(k, 'fileIsDir');
            content[fileName] = {'fileName':fileName, 'filePath':filePath, 'fileMD5':fileMD5, 'fileIsDir':fileIsDir };
        }
        return content
    }

    function viewDifference(fileName) {
        if (fileName in contentA && fileName in contentB && contentA[fileName].fileMD5 !== contentB[fileName].fileMD5) {
            selectedFileName = fileName
            main.state = 'skin';
        }
    }

    FileDialog {
        id: dialogDirectoryA
        title: 'Please choose a directory with old skins'
        folder: directoryA
        selectFolder: true
        onAccepted:  directoryA = fileUrl.toString().replace(/^file:\/\//, '')
    }

    FileDialog {
        id: dialogDirectoryB
        title: 'Please choose a directory with new skins'
        folder: directoryB
        selectFolder: true
        onAccepted:  directoryB = fileUrl.toString().replace(/^file:\/\//, '')
    }

    FolderListModel {
        id: modelDirectoryA
        folder: directoryA
        nameFilters: ["*.qml"]
        showDirsFirst: true
        showDotAndDotDot: true
        onDataChanged: contentA = getDirectoryContent(this)
        onModelReset: contentA = getDirectoryContent(this)
    }

    FolderListModel {
        id: modelDirectoryB
        folder: directoryB
        nameFilters: ["*.qml"]
        showDirsFirst: true
        showDotAndDotDot: true
        onDataChanged: contentB = getDirectoryContent(this)
        onModelReset: contentB = getDirectoryContent(this)
    }

    Row {
        id: tabButtons
        height: 40
        width: parent.width
        TabButton {
            id: fileViewButton
            height: parent.height
            width: 40
            icon.source: 'assets/Skins_icon.svg'
            onPressed: main.state = 'skin'
        }
    }

    /// Directory A
    Rectangle {
        id: textDirectoryA
        width: parent.width / 2
        anchors.left: parent.left
        anchors.top: tabButtons.bottom
        anchors.margins: 5
        height: childrenRect.height
        color: '#ffd0d0'
        Text { text: directoryA; font.pixelSize: 18; }
        MouseArea { anchors.fill: parent; onPressed: dialogDirectoryA.visible = true; }
    }

    ListView {
        id: viewDirectoryA
        anchors.left: textDirectoryA.left
        anchors.right: textDirectoryA.right
        anchors.top: textDirectoryA.bottom
        anchors.bottom: parent.bottom
        anchors.topMargin: 10

        model: modelDirectoryA
        delegate: Rectangle {
            width: viewDirectoryA.width
            height: childrenRect.height
            color: fileName === highlightedName ? highlightedColor : 'transparent'
            Text {
                text: fileName
                font.pixelSize: 18
                color: fileIsDir ? 'black' : (contentB && fileName in contentB) ? (contentB[fileName].fileMD5 === helper.md5(filePath) ? 'gray' : 'black' ) : 'red'
                font.strikeout: typeof contentB !== 'undefined'  && !(fileName in contentB)
                font.bold: fileName === selectedFileName
            }
            MouseArea {
                id: mouseArea
                anchors.fill: parent
                hoverEnabled: true
                onEntered: highlightedName = fileName
                onExited: highlightedName = ''
                onPressed: if (fileIsDir) directoryA = helper.absolutePath(directoryA + '/' + fileName); else viewDifference(fileName)
            }
        }
    }

    /// Directory B
    Rectangle {
        id: textDirectoryB
        width: parent.width / 2
        anchors.left: textDirectoryA.right
        anchors.top: tabButtons.bottom
        anchors.margins: 5
        height: childrenRect.height
        color: '#d0ffd0'
        Text { text: directoryB; font.pixelSize: 18; }
        MouseArea { anchors.fill: parent; onPressed: dialogDirectoryB.visible = true; }
    }

    ListView {
        id: viewDirectoryB
        anchors.left: textDirectoryB.left
        anchors.right: textDirectoryB.right
        anchors.top: textDirectoryB.bottom
        anchors.topMargin: 10
        anchors.bottom: parent.bottom

        model: modelDirectoryB
        delegate: Rectangle {
            width: viewDirectoryB.width
            height: childrenRect.height
            color: fileName === highlightedName ? highlightedColor : 'transparent'
            Text {
                text: fileName
                font.pixelSize: 18
                color: fileIsDir ? 'black' : (contentA && fileName in contentA) ? (contentA[fileName].fileMD5 === helper.md5(filePath) ? 'gray' : 'black' ) : 'green'
                font.bold: fileName === selectedFileName
            }
            MouseArea {
                id: mouseArea
                anchors.fill: parent
                hoverEnabled: true
                onEntered: highlightedName = fileName
                onExited: highlightedName = ''
                onPressed: if (fileIsDir) directoryB = helper.absolutePath(directoryB + '/' + fileName); else viewDifference(fileName)
            }
        }
    }
}
