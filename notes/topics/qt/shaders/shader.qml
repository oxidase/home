import QtQuick 2.0

Rectangle {
    width: 512
    height: 512

    MouseArea {
        anchors.fill: parent
        onClicked: {
            Qt.quit();
        }
    }

    Image {
        id: kitty
        source: "kitty.png"
    }

    Item {
        id: scrollercontainer
        anchors.fill: parent

        Text {
            id: scroller
            text: "Hello QtQuick world!"
            color: "green"
            font.pixelSize: 80
            anchors.verticalCenter: parent.verticalCenter
        }
    }

    ShaderEffectSource {
        id: kitty_source
        sourceItem: kitty
    }

    ShaderEffectSource {
        id: scroller_source
        sourceItem: scrollercontainer
    }

    ShaderEffect {
        id: kitty_effect
        anchors.fill: parent

        property real angle : 0.0
        PropertyAnimation on angle {
            to: 360.0
            duration: 1800
            loops: Animation.Infinite
        }

        property variant source : kitty_source

        fragmentShader: "
        uniform highp float angle;
        uniform lowp sampler2D source;
        varying highp vec2 qt_TexCoord0;
        void main() {
            highp float texAngle = 0.0;
            if (qt_TexCoord0.x != 0.0 || qt_TexCoord0.y != 0.0) {
                texAngle = atan(qt_TexCoord0.y - 0.5, qt_TexCoord0.x - 0.5);
            }
            highp float skew = sqrt(pow(qt_TexCoord0.x - 0.5, 2.0)
                                    + pow(qt_TexCoord0.y - 0.5, 2.0))
                               * 10.0;
            highp vec4 colorwheel = vec4(sin(texAngle + radians(angle) - skew),
                                   sin(texAngle + radians(angle - 120.0) - skew),
                                   sin(texAngle + radians(angle - 240.0) - skew),
                                   1.0);
            highp float wavefactor = 0.03;
            highp float wave_x = qt_TexCoord0.x + wavefactor
                           * sin(radians(angle + qt_TexCoord0.x * 360.0));
            highp float wave_y = qt_TexCoord0.y + wavefactor
                           * cos(radians(angle + qt_TexCoord0.y * 360.0));
            highp vec4 texpixel = texture2D(source, vec2(wave_x, wave_y));
            gl_FragColor = colorwheel * texpixel;
        }"
    }

}