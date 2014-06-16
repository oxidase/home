import QtQuick 2.0

Rectangle {
    width: 800
    height: 450

    property real time
    NumberAnimation on time { from:0;to:100;duration:100000;loops:Animation.Infinite;running:true}

    ShaderEffectSource {
        id: rasp
        smooth: true
        sourceItem: Image { source:"Raspberry.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
        mipmap: true
    }
    ShaderEffectSource {
        id: qtlogo
        smooth: true
        sourceItem: Image { source:"qt-logo.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
        mipmap: true
    }

    ShaderEffect {
        width: parent.width
        height: parent.height
        property variant r: rasp
        property variant q: qtlogo
        property real time: parent.time
        fragmentShader: "
        varying highp vec2 qt_TexCoord0;
        uniform sampler2D r;
        uniform sampler2D q;
        uniform float time;
        void main(void)
        {
            highp vec2 scale = vec2(16.0/9.0,1.0);
            highp vec2 offset = vec2(-0.7,abs(sin(time*3.))*0.2);
            highp vec2 coords = clamp(qt_TexCoord0*scale+offset,0.,1.);
            lowp vec4 rcol = texture2D(r,coords);
            gl_FragColor = rcol*vec4(1.0,1.0,1.0,1.0);
            // gl_FragColor = vec4(0,coords.y,.0,1.0);
            // gl_FragColor = vec4(texture2D(r,qt_TexCoord0*vec2(16.0/9.0,1.0)+vec2(-0.2,abs(sin(time*3.))*0.2)).rgb,1.0);
        }"
        Text {
            text: "This is a\nRaspberry"
            color: "red"
            font.pixelSize: 20
            x: 100; y: 400
        }
    }
}
