import QtQuick 2.0

Rectangle {
    width: 360
    height: 360
    ShaderEffect {
        anchors.fill: parent
        fragmentShader: "
            varying highp vec2 qt_TexCoord0;
            void main(void)
            {
                lowp vec4 c0 = vec4( 1.0, 1.0, 1.0, 1.0 );
                lowp vec4 c1 = vec4( 1.0, 0.0, 0.0, 1.0 );
                gl_FragColor = mix( c0, c1, qt_TexCoord0.y );
                // gl_FragColor = vec4(qt_TexCoord0, 0, 1);
            }
        "
    }
}
