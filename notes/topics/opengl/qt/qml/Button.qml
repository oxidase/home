import QtQuick 2.0

Item
{
    width: but.width
    height: but.height
    property alias text: textItem.text

    Rectangle {
        id: but
        width: 130;
        height: 40
        border.width: 1
        radius: 5
        smooth: true

        gradient: Gradient {
            GradientStop { position: 0.0; color: "darkGray" }
            GradientStop { position: 0.5; color: "black" }
            GradientStop { position: 1.0; color: "darkGray" }
        }

        Text {
            id: textItem
            anchors.centerIn: parent
            font.pointSize: 20
            color: "white"
        }

        MouseArea {
            property bool ent: false
            id: moousearea
            anchors.fill: parent
            onEntered: {
                ent = true
            }
            onExited: {
                ent = false
                effect.angle = 0.0
            }
            hoverEnabled: true
        }
    }

    ShaderEffect {
        id: effect
        property variant source: ShaderEffectSource {
            sourceItem: but;
            hideSource: true
        }
        anchors.fill: but
        property real angle : 0.0
        PropertyAnimation on angle {
            id: prop1
            from: 0.0
            to: 360.0
            duration: 800
            loops: Animation.Infinite
            running: moousearea.ent
        }

        fragmentShader: "
         varying highp vec2 qt_TexCoord0;
         uniform lowp sampler2D source;
         uniform highp float angle;
         void main() {
             highp float wave = 0.02;
             highp float wave_x = qt_TexCoord0.x + wave * sin( radians( angle + qt_TexCoord0.x * 360.0 ) );
             highp float wave_y = qt_TexCoord0.y + wave * sin( radians( angle + qt_TexCoord0.y * 360.0 ) );
             highp vec4 texpixel = texture2D( source, vec2( wave_x, wave_y ) );
             gl_FragColor = texpixel;
         }"
    }
}