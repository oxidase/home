import QtQuick 2.0;
import QtLocation 5.0

MapQuickItem {
    id: marker
    objectName: 'routeWaypoint'
    z: map.z+1
    anchorPoint.x: image.width/2
    anchorPoint.y: image.height

    sourceItem: Image {
        property alias text: label.text
        
        id: image
        source: "../images/balloon.svg"

        Text {
            id: label
            x: 0; y: 0
            width: parent.width
            height: parent.height - 5
            font.pointSize: 10
            wrapMode: Text.Wrap
            clip: true
        }
    }

    GeocodeModel {
        property variant lastCoordinate: undefined

        id: geocodeModel
        plugin: map.plugin
        onStatusChanged: {
            if (status == GeocodeModel.Ready) {
                if (count > 0) {
                    image.text = geocodeModel.get(0).address.text;
                } else {
                    image.text = "No results";
                }
            } else if (status == GeocodeModel.Loading && lastCoordinate === undefined) {
                image.text = "Loading...";
            } else if (status == GeocodeModel.Error) {
                console.log('Error: ' + geocodeModel.errorString)
            }
        }
    }

    MouseArea {
        property double bound: 0.1

        z: parent.z
        anchors.fill: parent
        drag.target: marker
        hoverEnabled: true
        
        onEntered: map.gesture.enabled = false
        onExited: map.gesture.enabled = true
        onPositionChanged: {
            if (!drag.active)
                 return;

            var x = Math.max(0, Math.min(map.width, marker.x + anchorPoint.x));
            var y = Math.max(0, Math.min(map.height, marker.y + anchorPoint.y));
            var boundX = bound * map.width;
            var boundY = bound * map.height;
            var stepX = bound * map.widthInMeters(y);
            var stepY = bound * map.heightInMeters(x);
            if (x < boundX) {
                var scaleX = Math.min((boundX-x)/boundX, 1);
                map.center = map.center.atDistanceAndAzimuth(stepX*scaleX, 270);
            } else if (x > map.width - boundX) {
                var scaleX = Math.min((x-(map.width-boundX))/boundX, 1);
                map.center = map.center.atDistanceAndAzimuth(stepX*scaleX, 90);
            }
            if (y < boundY) {
                var scaleY = Math.min((boundY-y)/boundY, 1);
                map.center = map.center.atDistanceAndAzimuth(stepY*scaleY, 0);
            } else if (y > map.height - boundY) {
                var scaleY = (y-(map.height-boundY))/boundY;
                map.center = map.center.atDistanceAndAzimuth(stepY*scaleY, 180);
            }
            marker.coordinate = map.toCoordinate(Qt.point(x, y))
        }
    }

    Timer {
        id: updateTimer
        interval: 1000
        onTriggered: {
            if (geocodeModel.query != geocodeModel.lastCoordinate) {
                geocodeModel.lastCoordinate = coordinate;
                geocodeModel.update();
            }
        }
    }

    onCoordinateChanged: {
        if (geocodeModel.query == undefined || !updateTimer.running) {
            geocodeModel.query = coordinate;
            geocodeModel.lastCoordinate = coordinate;
            geocodeModel.update();
            updateTimer.start()
        } else {
            geocodeModel.query = coordinate;
        }
    }
}