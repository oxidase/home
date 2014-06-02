import QtQuick 2.0
import QtGraphicalEffects 1.0

GridView {
    id: callView
    anchors.fill: parent
    property int zoom: count <= 1 ? 1 : count <= 4 ? 2 : 3
    cellWidth: width / zoom
    cellHeight: height / zoom
    model: callManager.calls

    delegate: Rectangle {
        // color: "grey"
        border.color: "green"
        width: GridView.view.cellWidth
        height: GridView.view.cellHeight
        property var info: contactsModel.getCallInfo(path)
        property int currentDuration: 0
        Image {
            id: personPhoto
            anchors.top: parent.top
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: 0.05 * parent.height
            width: Math.min(300, 0.8 * parent.width)
            height: Math.min(300, 0.5 * parent.height)
            fillMode: Image.PreserveAspectCrop
            source: 'PHOTO' in info ? info['PHOTO'] : 'image://avatar/user'
        }
        Text {
            id: personName
            anchors.top: personPhoto.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: 0.025 * parent.height
            font.pixelSize: 48 / callView.zoom
            text:info && 'FN' in info ? info['FN'] : 'Unknown'
        }
        Text {
            id: personNumber
            anchors.top: personName.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: 0.025 * parent.height
            font.pixelSize: 32 / callView.zoom
            text: lineIdentification
        }
        Text {
            id: timerText
            property int seconds: (currentDuration / 1000) % 60
            property int minutes: (seconds / 60) % 60
            property int hours: (minutes / 60) % 24
            property int days: (hours / 24)
            anchors.top: personNumber.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: 0.025 * parent.height
            font.pixelSize: 32 / callView.zoom
            text: (days>1 ? days+' days ' : days===1 ? '1 day ' : '') +
                  (hours>=10 ? hours+':'  : hours>0 ? '0'+hours+':' : '') +
                  (minutes>=10 ? minutes+':'  : '0' + minutes+':') +
                  (seconds>=10 ? seconds : '0' + seconds)
            Timer {
                id: callTimer
                interval: 1000
                repeat: true
                triggeredOnStart: true
                running: true
                onTriggered: currentDuration = (new Date) - startTime;
            }
        }
        Text {
            id: stateText
            anchors.top: timerText.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.topMargin: 0.025 * parent.height
            font.pixelSize: 24 / callView.zoom
            text: model.state
        }
        Row {
            anchors.bottom: parent.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.bottomMargin: 0.05 * parent.height
            height: Math.min(64, 0.1 * parent.height)
            spacing: 0.1*parent.width
            Item {
                id: releaseAndAnswerButton
                height: parent.height
                width: phoneReleaseAndAnswer.width
                visible: model.state === 'waiting'
                Image {
                    id: phoneReleaseAndAnswer
                    height: parent.height
                    fillMode: Image.PreserveAspectFit
                    source: '../assets/phone_releaseAndAnswer.png'
                }
                MouseArea {
                    id: phoneReleaseAndAnswerArea
                    anchors.fill: phoneReleaseAndAnswer
                    hoverEnabled: true
                    onPressed: callManager.releaseAndAnswer()
                }
            }
            Item {
                id: answerButton
                height: parent.height
                width: phoneAnswer.width
                visible: model.state === 'incoming' || model.state === 'waiting'
                Image {
                    id: phoneAnswer
                    height: parent.height
                    fillMode: Image.PreserveAspectFit
                    source: model.state === 'incoming' ? '../assets/phone_selected.png' : '../assets/phone_holdAndAnswer.png'
                }
                DropShadow {
                    anchors.fill: phoneAnswer
                    horizontalOffset: 4
                    verticalOffset: 4
                    radius: 4
                    samples: 8
                    color: "black"
                    transparentBorder: true
                    source: phoneAnswer
                    visible: phoneAnswerArea.containsMouse
                }
                MouseArea {
                    id: phoneAnswerArea
                    anchors.fill: phoneAnswer
                    hoverEnabled: true
                    onPressed: model.state === 'incoming' ? callManager.calls.answer(path) : callManager.holdAndAnswer()
                }
            }
            Item {
                id: hangupButton
                height: parent.height
                width: phoneHangup.width
                Image {
                    id: phoneHangup
                    height: parent.height
                    fillMode: Image.PreserveAspectFit
                    source: '../assets/phone_hangup.png'
                }
                Glow {
                    anchors.fill: phoneHangup
                    radius: 8
                    samples: 16
                    source: phoneHangup
                    transparentBorder: true
                    color: "red"
                    visible: phoneHangupArea.containsMouse
                }
                MouseArea {
                    id: phoneHangupArea
                    anchors.fill: phoneHangup
                    hoverEnabled: true
                    onPressed: callManager.calls.hangup(path)
                }
            }
        }
    }

    Keys.onPressed: {
        if (event.key === Qt.Key_Escape) callManager.hangupAll()
    }
}