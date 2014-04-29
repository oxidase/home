import QtQuick 2.0

Item {
    anchors.fill: parent

    Image {
        id: personPhoto
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.topMargin: 200
        width: 300
        height: 300
    }
    Text {
        id: personName
        anchors.top: personPhoto.bottom
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.topMargin: 20
        font.pixelSize: 48
    }
    Text {
        id: personNumber
        anchors.top: personName.bottom
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.topMargin: 20
        font.pixelSize: 32
    }

    Text {
        property alias ms: callTimer.currentDuration
        property int seconds: (ms / 1000) % 60
        property int minutes: (seconds / 60) % 60
        property int hours: (minutes / 60) % 24
        property int days: (hours / 24)
        id: timerText
        anchors.top: personNumber.bottom
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.topMargin: 40
        font.pixelSize: 32
        text: (days>1 ? days+' days ' : days===1 ? '1 day ' : '') +
              (hours>=10 ? hours+':'  : hours>0 ? '0'+hours+':' : '') +
              (minutes>=10 ? minutes+':'  : '0' + minutes+':') +
              (seconds>=10 ? seconds : '0' + seconds)
    }
    Timer {
        id: callTimer
        property var startTime: null
        property int currentDuration: 0
        interval: 1000
        repeat: true
        triggeredOnStart: true
        running: false
        onRunningChanged: if (running) startTime = new Date
        onTriggered: currentDuration = (new Date) - startTime
    }

    Keys.onPressed: {
        if (event.key === Qt.Key_Escape) callManager.hangupAll()
    }

    // Component.onCompleted: callTimer.running = true

    Connections {
        target: callManager
        onCallAdded: {
            callTimer.running = true
            var info = contactsModel.getCallInfo(call)
            personName.text = info['FN']
            personNumber.text = info['TEL']
            personPhoto.source = 'PHOTO' in info ? info['PHOTO'] : 'image://avatar/user'
        }
        onCallRemoved: callTimer.running = false
        onHangupAllComplete: callTimer.running = false
    }
    
}