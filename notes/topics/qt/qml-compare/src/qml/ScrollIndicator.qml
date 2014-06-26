import QtQuick 2.0

Item {
    id: scrollInd

    signal dragPosition(real position)

    property real position: 0.5
    property real zoom: 0.5
    // Specifies whether the scroll indicator is shown
    property bool shown: false
    property bool hover: false
    property int knob_height: 150

    // Return the y position of the knob in
    // ScrollIndicator coordinate system
    function getY(position) {
        if (zoom == 1 || position < 0)
            return 0;
        else if (position > 1 - zoom)
            return scrollInd.height - scroll_knob.height;
        else
            return position / (1 - zoom) *(scrollInd.height - scroll_knob.height);
    }

    width: scroll_knob.width

    MouseArea {
        anchors.fill: parent
        hoverEnabled: true
        onEntered: hover = true
        onExited: hover = false
        onClicked: {
            if (mouse.y > scroll_knob.y + scroll_knob.height) {
                dragPosition(Math.min((scroll_knob.y + scroll_knob.height) / scrollInd.height, 1))
            } else if (mouse.y < scroll_knob.y) {
                dragPosition(Math.max((scroll_knob.y - scroll_knob.height) / scrollInd.height, 0))
            }
        }
    }

    // Background area
    Rectangle {
        id: scroll_bar
        opacity: 0
        x: scroll_knob.width / 2 - width /2
        y: scroll_knob.height / 2
        width: 6
        height: parent.height - scroll_knob.height
        border.color: "white"
        border.width: 2
        color: "black"
        radius: 2
    }

    // Knob that displays the relative position
    Rectangle {
        id: scroll_knob
        opacity: 0
        y: Math.max(0, getY(position))
        width: 12
        height: knob_height
        color: "gray"

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            onEntered: hover = true
            onExited: if (!drag.active) hover = false
            drag.target: parent
            drag.axis: Drag.YAxis
            drag.minimumY: 0
            drag.maximumY: scrollInd.height - parent.height
            onPositionChanged: {
                if (!drag.active)
                    return
                var reduced_height = (scrollInd.height - parent.height)
                var position = Math.max(0, Math.min(parent.y, reduced_height)) / reduced_height
                dragPosition(position)
            }
            drag.onActiveChanged: if (!drag.active && !containsMouse) hover = false
        }
    }

    // ScrollIndicator has 'shown' state in addition
    // to the default state (when the component is invisible)
    states: [
        State {
            name: "shown"
            when: shown || hover
            PropertyChanges { target: scroll_knob; opacity: 1 }
            PropertyChanges { target: scroll_bar; opacity: 0.2 }
        }
    ]

    transitions: [
        Transition {
            NumberAnimation { properties: "opacity"; easing.type: Easing.InOutQuad; duration: 400 }
        }
    ]
}
