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

    Rectangle {
        anchors.fill: parent
        color: "gray"
    }

    Item {
        id: theItem
        anchors.centerIn: parent
        rotation: 0
        Image {
            id: kitty
            anchors.centerIn: parent
            source: "kitty.png"
        }
        Text { anchors.top: kitty.bottom; text: "Hello, Kitty" }
    }

    ShaderEffectSource {
        id: theSource
        sourceItem: theItem
    }

    ShaderEffectSource {
        id: recursiveSource
        sourceItem: kitty_effect
        hideSource: false
        visible: false
        live: true
        recursive: true
        smooth: false
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

        property variant source : theSource
        property variant recursiveSource: recursiveSource

        vertexShader: "
        uniform highp mat4 qt_Matrix;
        attribute highp vec4 qt_Vertex;
        attribute highp vec2 qt_MultiTexCoord0;
        varying highp vec2 qt_TexCoord0;
        void main() {
            qt_TexCoord0 = qt_MultiTexCoord0;
            gl_Position = qt_Matrix * qt_Vertex;
        }"

        fragmentShader: "
        uniform highp float angle;
        uniform lowp sampler2D source;
        uniform lowp sampler2D recursiveSource;
        varying highp vec2 qt_TexCoord0;
        void main() {
            // highp float texAngle = 0.0;
            // if (qt_TexCoord0.x != 0.0 || qt_TexCoord0.y != 0.0) {
            //     texAngle = atan(qt_TexCoord0.y - 0.5, qt_TexCoord0.x - 0.5);
            // }
            // highp float skew = sqrt(pow(qt_TexCoord0.x - 0.5, 2.0)
            //                         + pow(qt_TexCoord0.y - 0.5, 2.0))
            //                    * 10.0;
            // highp vec4 colorwheel = vec4(sin(texAngle + radians(angle) - skew),
            //                        sin(texAngle + radians(angle - 120.0) - skew),
            //                        sin(texAngle + radians(angle - 240.0) - skew),
            //                        1.0);
            // highp float wavefactor = 0.03;
            // highp float wave_x = qt_TexCoord0.x + wavefactor
            //                * sin(radians(angle + qt_TexCoord0.x * 360.0));
            // highp float wave_y = qt_TexCoord0.y + wavefactor
            //                * cos(radians(angle + qt_TexCoord0.y * 360.0));
            // highp vec4 texpixel = texture2D(source, vec2(wave_x, wave_y));
            // gl_FragColor = vec4(1,0,0,1); //colorwheel * texpixel;
            gl_FragColor = texture2D(source, qt_TexCoord0.xy-vec2(0.5,0.5));
        }"
    }
}