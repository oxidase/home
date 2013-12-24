import QtQuick 2.0

Rectangle {
     width: img.width
     height: img.height

     Image {
        id: img
        source: "nature.jpg"
     }

     ShaderEffect {
         id: effect
         anchors.fill: parent
         MouseArea {
             id: coords
             hoverEnabled: true
             anchors.fill: parent
             onPositionChanged: {
                 console.log(effect.xPos, effect.yPos)
                 effect.xPos = mouse.x
                 effect.yPos = mouse.y
             }
         }
         property real xPos: 65.0
         property real yPos: 65.0
         property real radius: 50
         property int widthImage: img.width
         property int heightImage: img.height
         property variant source: ShaderEffectSource {
             sourceItem: img;
             hideSource: true
         }

         fragmentShader:
         "varying highp vec2 qt_TexCoord0;
         uniform highp float xPos;
         uniform highp float yPos;
         uniform highp float radius;
         uniform highp int widthImage;
         uniform highp int heightImage;
         highp vec2 pixcoords = qt_TexCoord0.st * vec2(widthImage, heightImage);
         uniform sampler2D source;
         void main(void)
         {
             lowp vec4 texColor = texture2D(source, qt_TexCoord0.st);
             lowp float gray = dot( texColor, vec4( 0.2126, 0.7152, 0.0722, 0.0 ) );
             if ( ( pow((xPos - pixcoords.x), 2) + pow((yPos - pixcoords.y ), 2)) < pow(radius, 2)) {
                 gl_FragColor = vec4( gray, gray, gray, texColor.a) ;
             } else {
                 gl_FragColor = texture2D( source, qt_TexCoord0 );
             }
          }"
    }
 }