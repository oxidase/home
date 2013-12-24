import QtQuick 2.0

Item {
     width: img.width
     height: img.height

     Image {
          id: img
          source: "moon.jpg"
     }

     ShaderEffect {
         id: shader
         property variant source: ShaderEffectSource {
             sourceItem: img;
             hideSource: true
         }
         anchors.fill: img
         fragmentShader: "
         varying highp vec2 qt_TexCoord0;
         uniform lowp sampler2D source;
         uniform highp float angle;
         void main() {
             highp float wave = 0.01;
             highp float wave_x = qt_TexCoord0.x + wave * sin( radians( angle + qt_TexCoord0.x * 360.0 ) );
             highp float wave_y = qt_TexCoord0.y + wave * sin( radians( angle + qt_TexCoord0.y * 360.0 ) );
             highp vec4 texpixel = texture2D( source, vec2( wave_x, wave_y ) );
             gl_FragColor = texpixel;
         }"
         
         property real angle : 0.0
         PropertyAnimation on angle {
             from: 0.0
             to: 360.0
             duration: 1800
             loops: Animation.Infinite
         }
     }

     Text {
         text: shader.angle
         color: "white"
         anchors.left: parent.left
         anchors.top: parent.top
         z:100
     }
}

