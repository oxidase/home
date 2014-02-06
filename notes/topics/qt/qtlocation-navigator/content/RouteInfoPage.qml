import QtQuick 2.0
import QtLocation 5.0
import QtPositioning 5.2
import Qt.labs.settings 1.0

Rectangle {
    id: routeInfoPage
    property RouteModel model: undefined
    property alias currentIndex: listViewRoutes.currentIndex

    anchors { right: parent.right; top: parent.top; bottom: parent.bottom }
    width: Math.floor(parent.width/2.2)
    opacity: 0.6

    ListView {
        id: listViewRoutes
        anchors {left: parent.left; right: parent.right; top: parent.top; bottom: btnClose.top}
        clip: true
        model: parent.model
        delegate: Rectangle {
            width: listViewRoutes.width
            height: childrenRect.height
            state: 'collapsed'
            property var segments: model.routeData.segments
            Text { id: textRoute; anchors.top: parent.top; text: "Route: " + (index+1)}
            Text { id: textDistance; anchors.top: textRoute.bottom;  text: "Distance: " + model.routeData.distance + ' meters'}
            Text { id: textTravelTime; anchors.top: textDistance.bottom; text: "Travel time: " + model.routeData.travelTime + ' seconds'}
            ListView {
                id: listViewSegments
                width: parent.width
                height: childrenRect.height
                anchors.top: textTravelTime.bottom
                model: parent.segments
                interactive: false
                delegate: Item {
                    width: listViewSegments.width
                    height: childrenRect.height + 5
                    Image {
                        id: maneuver
                        source: '../images/maneuvers/maneuver-'+model.modelData.maneuver.direction+'.png'
                    }
                    Text{
                        id: instructions
                        anchors.left: maneuver.right
                        text: model.modelData.maneuver.instructionText

                    }
                    Text{
                        id: info
                        anchors { left: maneuver.right; top: instructions.bottom }
                        text: model.modelData.maneuver.distanceToNextInstruction + ' meters, ' + model.modelData.maneuver.timeToNextInstruction + ' seconds'
                    }
                    MouseArea {
                        width: parent.width
                        height: instructions.height + info.height
                        onClicked: {
                            // for(var k in model.modelData.maneuver) console.log(k, model.modelData.maneuver[k]);
                            map.center = model.modelData.maneuver.position
                            map.zoomLevel = 15
                        }
                    }
                }
            }
            states: [ State {
                name: 'collapsed'
                when: index !== listViewRoutes.currentIndex
                PropertyChanges { target: listViewSegments; height: 0; opacity: 0 }
            }]
            transitions: [ Transition {
                NumberAnimation { target: listViewSegments; property: "height"; duration: 400 }
                NumberAnimation { target: listViewSegments; property: "opacity"; duration: 400 }
            }]
            MouseArea {
                width: parent.width
                height: textRoute.height + textDistance.height + textTravelTime.height
                onClicked: listViewRoutes.currentIndex = index
            }
        }
    }

    Rectangle {
        id: btnClose
        anchors {left: parent.left; right: parent.right; bottom: parent.bottom}
        height: 40
        Text {anchors.centerIn: parent; text: "Close"}
        MouseArea {anchors.fill: parent; onClicked: routeInfoPage.visible = false; }
    }
}