import QtQuick 2.0

Item {
    id: root
    width: text.width/2
    height: text.height
    
    Text {
        id: text
        font.pixelSize: 72
        text: "Hello, world!"
    }
     
     ShaderEffect {
         id: shader
         property variant source: ShaderEffectSource {
             sourceItem: text;
             hideSource: true
         }
         anchors.fill: root
         fragmentShader: "
         varying highp vec2 qt_TexCoord0;
         uniform lowp sampler2D source;
         uniform int x_pos;
         uniform int show_width;
         uniform int text_width;
         void main() {
             highp float x = (float(x_pos)+qt_TexCoord0.x*float(show_width))/float(text_width);
             highp vec4 texpixel = texture2D(source, vec2(x, qt_TexCoord0.y));
             gl_FragColor = texpixel;
         }"
         
         property int x_pos: 0
         property int show_width: root.width
         property int text_width: text.width
         PropertyAnimation on x_pos {
             from: 0.0
             to: text.width - root.width
             duration: 3000
             loops: Animation.Infinite
         }
     }
}

