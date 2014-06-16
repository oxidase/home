import QtQuick 2.0

Rectangle {
    width: 400
    height: 400

    property real time
    NumberAnimation on time { from:0;to:100;duration:100000;loops:Animation.Infinite;running:true}

    ShaderEffectSource {
        id: rasp
        smooth: true
        sourceItem: Image { source:"assets/Raspberry.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
        mipmap: true
    }
    ShaderEffectSource {
        id: qtlogo
        smooth: true
        sourceItem: Image { source:"assets/kitty.png"; smooth: true; }
        wrapMode: ShaderEffectSource.Repeat
        mipmap: true
    }

    ShaderEffect {
        width: parent.width
        height: parent.height
        property variant r: rasp
        property variant q: qtlogo
        property real time: parent.time
        property int index: shaders.length - 1
        property var shaders:['
            highp vec2 scale = vec2(16.0/9.0,1.0);
            highp vec2 offset = vec2(-0.7,abs(sin(time*3.))*0.2);
            highp vec2 coords = clamp(qt_TexCoord0*scale+offset,0.,1.);
            gl_FragColor = mix(texture2D(r, qt_TexCoord0), texture2D(q, qt_TexCoord0), (sin(5.*time)+1.)/2.);',

            'highp vec2 scale = vec2(16.0/9.0,1.0);
            highp vec2 offset = vec2(-0.7,abs(sin(time*3.))*0.2);
            highp vec2 coords = clamp(qt_TexCoord0*scale+offset,0.,1.);
            gl_FragColor = mix(texture2D(r, qt_TexCoord0), texture2D(q, qt_TexCoord0), abs(qt_TexCoord0.x - 0.5) < fract(time)/2. ? 0 : 1);',

            'highp vec2 p = -1.+2.*qt_TexCoord0;
            highp vec2 uv;
            float an = time * 0.25;
            float can = cos(time);
            float san = sin(time);
            float x = p.x*can - p.y*san;
            float y = p.x*san + p.y*can;
            float invabsy = 1./abs(y);
            uv.x = x;
            uv.y = y;
            uv.x = x*invabsy;
            uv.y = 5.*time + 1.*invabsy;
            highp vec4 wallcol = texture2D(r, uv);
            gl_FragColor = vec4(wallcol.rgb, wallcol.a*abs(y));',

            '
            float an = -time * 1.;
            float can = cos(an);
            float san = sin(an);

            vec2 blurVec = vec2(-san, can)*0.07;
            vec2 coords = clamp(2. * (qt_TexCoord0 - (vec2(0.25, 0.25) + 0.25*vec2(can, san))), 0., 1.);
            vec4 result = texture2D(r, coords);
            int nSamples = 5;
            for (int i = 1; i < nSamples; ++i) {
                vec2 offset = blurVec * (float(i) / float(nSamples - 1) - 0.5);
                result += texture2D(r, clamp(coords + offset, 0., 1.));
            }
            gl_FragColor = result / float(nSamples);',

            '

            // float an = -time * 1.;
            // float can = cos(an);
            // float san = sin(an);

            // vec2 blurVec = vec2(-san, can)*0.07;
            // vec2 coords = clamp(2. * (qt_TexCoord0 - (vec2(0.25, 0.25) + 0.25*vec2(can, san))), 0., 1.);
            // vec4 result = texture2D(r, coords);
            // int nSamples = 5;
            // for (int i = 1; i < nSamples; ++i) {
            //     vec2 offset = blurVec * (float(i) / float(nSamples - 1) - 0.5);
            //     result += texture2D(r, clamp(coords + offset, 0., 1.));
            // }

            float scale = 2.;
            vec2 offset = vec2(0.25, 0.25);
            vec2 doffset = vec2(0.15, 0.);
            vec4 result = vec4(0,0,0,0);
            int nsamples = 4;
            for (int i=0; i<nsamples; ++i) {
                vec4 color = texture2D(r, clamp(scale * (qt_TexCoord0 - offset - float(i)*doffset/float(nsamples)), 0., 1.));
                // if (result.a == 0. && color.a > 0.)
                //     result += vec4(color.rgb, color.a * (1. - float(i)/float(nsamples))) ;
                result = mix(result, color, color.a * (1. - float(i)/float(nsamples)));
            }
            gl_FragColor = result;'

            ]

        fragmentShader: "
        varying highp vec2 qt_TexCoord0;
        uniform sampler2D r;
        uniform sampler2D q;
        uniform float time;
        void main(void) {" +shaders[index]+"}";
        onFragmentShaderChanged: console.log(fragmentShader)
        Text {
            text: "This is a\nRaspberry"
            color: "red"
            font.pixelSize: 20
            x: 100; y: 400
        }

        MouseArea{
            anchors.fill: parent
            onClicked: parent.index = (parent.index+1) % parent.shaders.length
        }
    }

}
