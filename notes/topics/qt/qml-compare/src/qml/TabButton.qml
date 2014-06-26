import QtQuick 2.0

Rectangle {
    id: buttonRect
    property alias label: labelText
    property alias icon: labelIcon
    property bool selected: false
    property color defaultColor: 'white'
    property color selectedColor: 'gray'

    signal pressed();

    color: selected || mouseArea.containsMouse ? selectedColor : defaultColor
    Text {
        anchors.centerIn: parent
        id: labelText
    }
    Image {
        anchors.centerIn: parent
        id: labelIcon
    }
    Behavior on color {
        ColorAnimation { duration: 50 }
    }
    MouseArea {
        id: mouseArea
        anchors.fill: parent
        hoverEnabled: true
        onPressed: parent.pressed()
    }
}