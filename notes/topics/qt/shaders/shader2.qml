import QtQuick 2.0

Rectangle {
    height: 300
    width: height * 1.5
    color: "black"

    rotation: 0

    //42 = ~25fps
    Timer {
        interval: 42; running: true; repeat: true
        onTriggered: parent.rotation++;
    }

    Image {
        id: textLabel
        source: "kitty.png"
        anchors.fill: parent
    }

    MouseArea {
        id: mouseArea
        onPositionChanged: { myShader.mymouseX = mouseX; myShader.mymouseY = mouseY;}
        anchors.fill: parent
        hoverEnabled: true
        onClicked: Qt.quit();
    }

    ShaderEffect {
        id: myShader
        property variant source: ShaderEffectSource { sourceItem: textLabel; hideSource: true }
        property real r: 0.0;

        //controls amount of red in shader below
        SequentialAnimation on r {
            loops: Animation.Infinite
            NumberAnimation { to: 1.0; duration: 8192; easing.type: Easing.InCurve }
            NumberAnimation { to: 0.0; duration: 8192; easing.type: Easing.InCurve }
        }

        property real angle: Math.random(360*(new Date()).getSeconds()) % 360;
        property real time: (new Date()).getSeconds();
        property real mymouseX: 250;
        property real mymouseY: 50;
        property real resolutionX: parent.width;
        property real resolutionY: parent.height;


        Timer {
            interval: 42; running: true; repeat: true
            onTriggered: parent.time += 0.1;
        }



        anchors.fill: textLabel

        fragmentShader: "
        varying highp vec2 qt_TexCoord0;
        uniform sampler2D source;
        uniform lowp float r;
        uniform lowp float time;

        #define BLU_SPEED	0.05
        #define RED_SPEED	0.2
        #define GREEN_SPEED	1.2
        #define NUM_WAVY_LINES 6.0

        uniform float mymouseX;
        uniform float mymouseY;
        uniform float resolutionX;
        uniform float resolutionY;

        void main() {
            vec2 pos = qt_TexCoord0.xy;
            vec2 mse;
            pos.x-=0.5;
            pos.y-=0.5;

            //wavy 'white' lines
            for (float i = 1.0; i <= NUM_WAVY_LINES; i++) {
                float phi = 0.002;
                float t = time;
                if (pos.x < 0.0) {
                    t = t * -1.0;
                }

                float fx = exp(phi * i * abs(pos.x) * 1200.0) * sin(i * time/4.0 + abs(pos.x) * 5.0) / 20.0 * cos(i);
                float dist = abs(pos.y-fx)*25.0;
                gl_FragColor += vec4(0.1/dist, 0.2/dist, 0.3/dist, 1.0);// * texture2D(source,qt_TexCoord0);
            }

            // //mouse
            // vec2 p = qt_TexCoord0.st;
            // vec2 mm;
            // mm.x = mymouseX;
            // mm.y = mymouseY;
            // float dis = distance(p, mm);
            // vec3 color = vec3(0.05,0.075,0.05);
            // color -= dis * 0.0035 + (sin(time)/5.0);
            // gl_FragColor += vec4(color, 1.0) * texture2D(source,qt_TexCoord0);

            // //variable red
            // gl_FragColor+=vec4(r,0.5,0.5,1.0);

            // //blue line
            // float fx=sin(pos.x*5.0+time*BLU_SPEED+mse.x*BLU_SPEED*5.0)/8.0;
            // float dist=abs(pos.y-fx)*80.0;
            // gl_FragColor+=vec4(0.5/dist,0.5/dist,1.25/dist,1.0);

            // //red line
            // fx=sin(pos.x*pos.x+time*RED_SPEED*2.0+mse.x*RED_SPEED*5.0)/15.0;
            // dist=abs(pos.y-fx)*80.0;
            // gl_FragColor+=vec4(1.25/dist,0.5/dist,0.5/dist,1.0);

            // //green line
            // fx=sin(pos.x/2.0+time*GREEN_SPEED+mse.x*GREEN_SPEED*2.5)/4.0;
            // //dist=abs(pos.y-fx)*80.0;
            // dist=abs(pos.y-fx)*100.0;
            // gl_FragColor+=vec4(0.25/dist,1.25/dist,0.25/dist,1.0);

            // if (pos.y<=-0.2) {
            //     gl_FragColor+=(vec4(abs(pos.y+0.2),abs(pos.y+0.2),abs(pos.y+0.2),1.0));
            // }

            // gl_FragColor = texture2D(source, qt_TexCoord0);
        } "
    }
}