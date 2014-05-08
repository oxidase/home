import QtQuick 2.0
import Ofono 1.0
import QtGraphicalEffects 1.0
import "./qml"

Flipable {
    id: container
    width: 600
    height: 1000
    state: 'contacts'

    // Connections {
    //     target: callManager
    //     onCallAdded: state = 'call'
    //     onCallRemoved: state = 'contacts'
    //     onHangupAllComplete: state = 'contacts'
    // }

    front: ContactsView {
        id: contactView
        anchors.fill: parent
        view.focus: container.state === 'contacts'
    }

    back: CallView {
        id: callView
        anchors.fill: parent
        focus: container.state === 'call'
    }

    transform: Rotation {
        id: rotator
        origin.x: container.width / 2
        origin.y: container.height / 2
        axis.x: 0; axis.y: 1; axis.z: 0
        angle: 0
    }

    states: [
        State {
            name: "call"
            when: callView.count > 0
            PropertyChanges { target: rotator; angle: 180 }
            PropertyChanges { target: callView; z: 1 }
        },
        State {
            name: "contacts"
            PropertyChanges { target: rotator; angle: 0 }
            PropertyChanges { target: contactView; z: 1 }
        }
    ]

    onStateChanged: console.log('state',state)
    transitions: [
        Transition {
            NumberAnimation { target: rotator; properties: "angle"; easing.type: Easing.InOutQuad; duration: 500 }
        }
    ]
}
