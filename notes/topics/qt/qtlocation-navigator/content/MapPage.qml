import QtQuick 2.0
import QtLocation 5.0
import QtPositioning 5.2
import Qt.labs.settings 1.0

Map {
    id: map
    plugin: locationPlugin
    // gesture.onPinchStarted: console.log('onPinchStarted')
    // gesture.onPinchUpdated: console.log('onPinchUpdated')
    // gesture.onPinchFinished: console.log('onPinchFinished')
    // gesture.onPanStarted: console.log('panStarted')
    // gesture.onPanFinished: console.log('panFinished')
    // gesture.onFlickStarted: console.log('flickStarted')
    // gesture.onFlickFinished: console.log('onFlickFinished')
    // tilt: 30
    // bearing: 180

    function clear() {
        clearMapItems()
    }

    function route() {
        routeQuery.clearWaypoints();
        routeQuery.travelModes = RouteQuery.CarTravel
        routeQuery.routeOptimizations = RouteQuery.ShortestRoute
        for (var k in mapItems) {
            if (mapItems[k].objectName != 'routeWaypoint')
                continue;
            routeQuery.addWaypoint(mapItems[k].coordinate)
        }
        routeModel.update();
    }

    function fitToBoundingBox(box) {
        if (typeof box === undefined)
            return;
        // position camera to the center of bounding box
        center = box.center
        // adjust zoom
        var boxWidth = box.center.distanceTo(QtPositioning.coordinate())
        var boxHeight = box.height*111000;
        var mapWidth = widthInMeters(0), mapHeight = heightInMeters(0);
        var boxWidthRatio = boxWidth / (boxWidth + boxHeight);
        var mapWidthRatio = mapWidth / (mapWidth + mapHeight);
        var zoomRatio = boxWidthRatio > mapWidthRatio ?
                        mapWidth / boxWidth :
                        mapHeight / boxHeight;
        var newZoom = Math.log(zoomRatio) / Math.log(2);
        zoomLevel = Math.min(maximumZoomLevel, Math.max(minimumZoomLevel, Math.floor(zoomLevel + newZoom)));
    }

    function widthInMeters(y) {
        var coord1 = map.toCoordinate(Qt.point(0, y));
        var coord2 = map.toCoordinate(Qt.point(width, y));
        return Math.round(coord1.distanceTo(coord2));
    }

    function heightInMeters(x) {
        var coord1 = map.toCoordinate(Qt.point(x, 0));
        var coord2 = map.toCoordinate(Qt.point(x, height));
        return Math.round(coord1.distanceTo(coord2));
    }

    Settings {
        category: "Map"
        property alias zoomLevel: map.zoomLevel
        property alias latitude: map.center.latitude
        property alias longitude: map.center.longitude
    }

    RouteQuery {
        id: routeQuery
        numberAlternativeRoutes: 2
    }

    RouteModel {
        id: routeModel
        plugin : map.plugin
        query: routeQuery
        onStatusChanged: {
            if (status == RouteModel.Ready) {
                if (count > 0) {
                    routeInfoModel.update(routeModel.get(0));
                    fitToBoundingBox(routeModel.get(0).bounds);
                    routeInfoPage.currentIndex = 0;
                    routeInfoPage.visible = true;
                }
            } else if (status == RouteModel.Error) {
                console.log('RouteModel error =', routeModel.errorString);
            }
        }
    }

    MapItemView {
        model: routeModel
        autoFitViewport: true
        delegate: MapRoute {
            route: routeData

            line.color: routeMouseArea.containsMouse ? "lime" : "red"
            line.width: 5
            smooth: true
            visible: index === routeInfoPage.currentIndex
            MouseArea {
                id: routeMouseArea
                anchors.fill: parent
                hoverEnabled: false
            }
        }
    }

    RouteInfoPage {
        id: routeInfoPage
        visible: false
        z: map.z + 2
        model: routeModel
    }

    ListModel {
        id: routeInfoModel

        property string travelTime
        property string distance

        function update(route) {
            clear()
            for (var i = 0; i < route.segments.length; i++) {
                append({
                    "instruction": route.segments[i].maneuver.instructionText,
                    "distance": route.segments[i].maneuver.distanceToNextInstruction
                });
            }
            travelTime = route.travelTime
            distance = route.distance
            console.log('travelTime = ', travelTime, 'distance = ', distance)
        }
    }

    MouseArea {
        property bool positionHeld: false
        anchors.fill: parent
        onPressed: positionHeld = true
        onPositionChanged: positionHeld = false
        onPressAndHold: {
            if (positionHeld) {
                var marker = Qt.createQmlObject ('Marker {}', map)
                marker.coordinate = mouseToCoordinate(Qt.point(mouse.x, mouse.y))
                map.addMapItem(marker)
            }
        }
    }

    onCenterChanged: map.center = center

    Component.onCompleted: {
        var start = Qt.createQmlObject ('Marker {}', map)
        start.coordinate = QtPositioning.coordinate(50.3844,30.4799, 0)
        map.addMapItem(start)
        var finish = Qt.createQmlObject ('Marker {}', map)
        finish.coordinate = QtPositioning.coordinate(50.4641,30.3429,0)
        map.addMapItem(finish)
        fitViewportToMapItems()
    }
}