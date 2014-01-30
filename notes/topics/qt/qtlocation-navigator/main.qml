import QtQuick 2.1
import QtLocation 5.0
import QtQuick.Controls 1.1
import "content"

ApplicationWindow {
    visible: true
    width: 800
    height: 480

    Rectangle {
        color: "#212126"
        anchors.fill: parent
    }

    Plugin {
        id: locationPlugin
        name : "google"
    }

    MapPage {
        id: mapPage
        visible: false
    }

    StackView {
        id: stackView
        anchors.fill: parent
        initialItem: mapPage
    }

    Toolbar {
        id: toolbar
    }
    
    MouseArea {
        anchors {left: parent.left;  right: parent.right; bottom: parent.bottom}
        height: 10
        hoverEnabled: true
        onEntered: toolbar.show()
    }
    
    // // Implements back key navigation
    // Keys.onReleased: {
    //     console.log('ApplicationWindow.Keys.onReleased', event.key)
    //     if (event.key === Qt.Key_Back || event.key === Qt.Key_Escape) {
    //         if (stackView.depth > 1) {
    //             stackView.pop();
    //             event.accepted = true;
    //         } else { Qt.quit(); }
    //     }
    // }

    // toolBar: BorderImage {
    //     border.bottom: 8
    //     source: "images/toolbar.png"
    //     width: parent.width
    //     height: 100

    //     Rectangle {
    //         id: backButton
    //         width: opacity ? 60 : 0
    //         anchors.left: parent.left
    //         anchors.leftMargin: 20
    //         opacity: stackView.depth > 1 ? 1 : 0
    //         anchors.verticalCenter: parent.verticalCenter
    //         antialiasing: true
    //         height: 60
    //         radius: 4
    //         color: backmouse.pressed ? "#222" : "transparent"
    //         Behavior on opacity { NumberAnimation{} }
    //         Image {
    //             anchors.verticalCenter: parent.verticalCenter
    //             source: "images/arrow-left.png"
    //         }
    //         MouseArea {
    //             id: backmouse
    //             anchors.fill: parent
    //             anchors.margins: -10
    //             onClicked: stackView.pop()
    //         }
    //     }

    //     Text {
    //         font.pixelSize: 42
    //         Behavior on x { NumberAnimation{ easing.type: Easing.OutCubic} }
    //         x: backButton.x + backButton.width + 20
    //         anchors.verticalCenter: parent.verticalCenter
    //         color: "white"
    //         text: "Calendar"
    //     }
    // }

    // // LabeledComboBox {
    // //     id: calendar
    // //     width: parent.width
    // //     label.text: 'Calendar'
    // //     // combo.model: organizerModel.collections
    // //     // combo.textRole: 'name'
    // //     combo.model: 5
    // // }

    // //     LabeledTextField {
    // //         id: calendar1
    // //         anchors.top: calendar.bottom
    // //         width: parent.width
    // //         label.text: "What"
    // //         field.text: "Hiii"
    // //     }

    // MonthPage {
    //     id: monthPage
    //     visible: false
    // }

    // DayPage {
    //     id: dayPage
    //     visible: false
    // }
    
    // AgendaPage {
    //     id: agendaPage
    //     visible: false
    // }

    // EventPage {
    //     id: eventPage
    //     visible: false
    // }

    // StackView {
    //     id: stackView
    //     anchors.fill: parent
    //     initialItem: monthPage // agendaPage // dayPage
    // }


    
    // OrganizerModel {
    //     id: organizerModel
    //     // manager: "qtorganizer:kamaz:id=test&accountid=2"
    //     manager: "qtorganizer:mongodb"
    //     // startPeriod: Month.dropTime(new Date())
    //     // endPeriod: Month.tomorrow(startPeriod)
    //     startPeriod: new Date(2013, 12, 1, 0, 0)
    //     endPeriod: new Date(2014, 1, 10, 0, 0)

    //     signal itemsModified()
        
    //     property variant collectionsMap: {}
    //     onCollectionsChanged: {
    //         var map = {}
    //         for (var i=0; i<collections.length; ++i) {
    //             map[collections[i].collectionId] = {
    //                 'index': i,
    //                 'name': collections[i].name,
    //                 'color': collections[i].color
    //             };
    //         }
    //         collectionsMap = map;
    //     }

    //     onItemsChanged: itemsModified()
    // }
}
