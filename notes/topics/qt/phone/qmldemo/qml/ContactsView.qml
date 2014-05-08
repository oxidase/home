import QtQuick 2.0

Item {
    anchors.fill: parent
    property alias view: contactsView

    AnimatedImage {
        anchors.centerIn: parent
        source: "../assets/ajax-loader.gif"
        visible: contactsModel.loading
    }

    ListView {
        id: contactsView
        anchors { top: parent.top; bottom: dialLine.top; left: parent.left; right: parent.right}
        model: contactsModel
        flickDeceleration: 5000
        highlightMoveVelocity: 10000

        delegate: Rectangle {
            property bool isCurrentItem: ListView.isCurrentItem
            property string itemLabel: fn ? fn : email.length > 0 ? email[0] : vcfName
            property string itemAvatar: photo ? photo :
                                        tel && tel.length > 0 ? 'image://avatar/user' :
                                        email && email.length > 0 ? 'image://avatar/email' :
                                        'image://avatar/undefined'

            width: ListView.view.width
            height: childrenRect.height
            color: isCurrentItem ? "white" : "white"
            border.color: "gray"
            Item {
                visible: !isCurrentItem
                width: parent.width
                height: !isCurrentItem ? childrenRect.height : 0
                Image {
                    id: avatarSmall
                    anchors.top: parent.top
                    anchors.left: parent.left
                    anchors.topMargin: 1
                    anchors.leftMargin: 1
                    width: 64
                    height: 64
                    source: itemAvatar
                }
                Text {
                    id: nameSmall;
                    anchors.left: avatarSmall.right
                    anchors.leftMargin: 20
                    text: itemLabel
                    font.pixelSize: 20
                }
                Text {
                    anchors.top: nameSmall.bottom;
                    anchors.left: avatarSmall.right
                    anchors.leftMargin: 40
                    text: tel && tel.length > 0 ? tel[0] : fn && email && email.length > 0 ? email[0] : ''
                    font.pixelSize: 20
                }
                Behavior on height { NumberAnimation { duration: 100 } }
                MouseArea {
                    anchors.fill: parent
                    onClicked: contactsView.currentIndex = index
                }
            }

            Item {
                property bool completed: false
                visible: isCurrentItem
                width: parent.width
                height: visible ? childrenRect.height + 16 : 0
                clip: true
                Image {
                    id: avatarFull
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.top: parent.top
                    anchors.topMargin: 5
                    width: 200
                    height: 200
                    source: itemAvatar
                }
                Text {
                    id: nameFull
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.top: avatarFull.bottom
                    anchors.topMargin: 5
                    text: itemLabel
                    font.pixelSize: 30
                }
                ListView {
                    id: phoneFull
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.top: nameFull.bottom
                    anchors.topMargin: 5
                    width: 400
                    height: childrenRect.height
                    model: tel
                    delegate: Item{
                        width: ListView.view.width
                        height: phoneNumber.height + 4
                        Image {
                            id: phonePicto
                            anchors.left: parent.left
                            source: phoneArea.hover ? "../assets/phone_selected.png" : "../assets/phone.png"
                            height: phoneNumber.height
                            width: height
                        }
                        Text {
                            id: phoneNumber
                            anchors.left: phonePicto.right
                            anchors.leftMargin: 10
                            text: modelData
                            font.pixelSize: 20
                            color: phoneArea.hover ? "green" : "black"
                        }
                        MouseArea {
                            property bool hover: false
                            id: phoneArea
                            anchors.fill: parent
                            hoverEnabled: true
                            onEntered: hover = true
                            onExited: hover = false
                            onClicked: callManager.dial(modelData)
                        }
                    }
                }
                ListView {
                    id: emailFull
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.top: phoneFull.bottom
                    anchors.topMargin: 15
                    width: 400
                    height: childrenRect.height
                    model: email
                    delegate: Item{
                        width: ListView.view.width
                        height: childrenRect.height + 4
                        Image {
                            id: emailPicto
                            anchors.left: parent.left
                            source: "../assets/email.png"
                            height: emailText.height
                            width: height
                        }
                        Text {
                            id: emailText
                            anchors.left: emailPicto.right
                            anchors.leftMargin: 10
                            text: modelData
                            font.pixelSize: 20
                        }
                    }
                }
                Behavior on height { NumberAnimation { duration: 250 } }
                Component.onCompleted: childrenRect.height
            }
        }

        Keys.onPressed: {
            if (event.key === Qt.Key_PageDown) currentIndex = Math.min(currentIndex + 10, count - 1);
            else if(event.key === Qt.Key_PageUp) currentIndex = Math.max(currentIndex - 10, 0);
        }
    }

    ScrollIndicator {
        id: scroller
        visible: contactsView.visible
        anchors.right: parent.right
        anchors.top: contactsView.top
        anchors.bottom: contactsView.bottom
        position: contactsView.visibleArea.yPosition
        zoom: contactsView.visibleArea.heightRatio
        shown: contactsView.moving
        onDragPosition: contactsView.contentY = position * (contactsView.contentHeight - contactsView.height)
    }

    Rectangle {
        id: dialLine
        height: 32
        width: parent.width
        anchors.bottom: parent.bottom
        color: 'white'
        TextInput {
            id: callNumber
            height: parent.height
            anchors.left: parent.left
            anchors.right: callButton.left
            color: "green"
            text: ''
            font.pointSize: 20
        }
        Image{
            id: callButton
            height: parent.height
            width: height
            anchors.right: parent.right
            source: '../assets/phone_selected.png'
            MouseArea {
                anchors.fill: parent
                onPressed: callManager.dial(callNumber.text)
            }
        }
    }
}
