import QtQuick 2.0

Rectangle {
    width: backImage.width
    height: backImage.height

    property real time
    NumberAnimation on time { from:0;to:100;duration:100000;loops:Animation.Infinite;running:true}

    ShaderEffectSource {
        id: back
        smooth: true
        sourceItem: Image { id: backImage; source:"assets/water-background.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
    }
    ShaderEffectSource {
        id: water
        smooth: true
        sourceItem: Image { source:"assets/water-normal.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
    }
    ShaderEffectSource {
        id: displacement
        smooth: true
        sourceItem: Image { source:"assets/water-displacement.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
    }

    ShaderEffect {
        width: parent.width
        height: parent.height
        property variant back: back
        property variant water: water
        property variant disp: displacement
        property real time: parent.time

        fragmentShader: "
        varying highp vec2 qt_TexCoord0;
        uniform sampler2D back;
        uniform sampler2D water;
        uniform sampler2D disp;
        uniform float time;
        void main(void) {
            float timedelta = 0.;
            vec2 displacement = texture2D (disp, qt_TexCoord0).xy;
            // float t = clamp(qt_TexCoord0.y +1. - 1./height, 0.,1.);      // + displacement.y *0.1-0.15+ (sin (qt_TexCoord0.x * 60.0+timedelta) * 0.005);

            float height = 0.33 + displacement.x*0.02  + sin(60.*qt_TexCoord0.x+ 5.*time)*0.005;
            vec4 w = vec4(0.,0.,0.,0.);
            float t = (qt_TexCoord0.y - 1.) / height + 1.;
            if (t >= 0. && t < 1.)
                 gl_FragColor = (texture2D (back, qt_TexCoord0.xy) + texture2D(water, vec2(qt_TexCoord0.x, t)))/2.;
            //gl_FragColor = texture2D (back, qt_TexCoord0.xy);
            else
                gl_FragColor = texture2D (back, qt_TexCoord0.xy);
            // gl_FragColor = vec4(1.,0.,0.,1.)*w;
        }";

        MouseArea{
            anchors.fill: parent
            onClicked: parent.index = (parent.index+1) % parent.shaders.length
        }
    }

}
