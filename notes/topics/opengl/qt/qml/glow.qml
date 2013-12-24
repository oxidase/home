import QtQuick 2.0

Rectangle {
     width: img.width
     height: img.height

     Image {
        id: img
        source: "test.png"
     }

     ShaderEffect {
         id: effect
         anchors.fill: parent
         property real step_w: 1./img.width
         property real step_h: 1./img.height
         property real dividerValue: .9
         property variant source: ShaderEffectSource {
             sourceItem: img;
             hideSource: true
         }

         fragmentShader:"
         uniform float dividerValue;
         uniform float step_w;
         uniform float step_h;
         uniform sampler2D source;
         uniform lowp float qt_Opacity;
         varying vec2 qt_TexCoord0;
         void main()
         {
             vec2 uv = qt_TexCoord0.xy;
             vec3 t1 = texture2D(source, vec2(uv.x - step_w, uv.y - step_h)).rgb;
             vec3 t2 = texture2D(source, vec2(uv.x, uv.y - step_h)).rgb;
             vec3 t3 = texture2D(source, vec2(uv.x + step_w, uv.y - step_h)).rgb;
             vec3 t4 = texture2D(source, vec2(uv.x - step_w, uv.y)).rgb;
             vec3 t5 = texture2D(source, uv).rgb;
             vec3 t6 = texture2D(source, vec2(uv.x + step_w, uv.y)).rgb;
             vec3 t7 = texture2D(source, vec2(uv.x - step_w, uv.y + step_h)).rgb;
             vec3 t8 = texture2D(source, vec2(uv.x, uv.y + step_h)).rgb;
             vec3 t9 = texture2D(source, vec2(uv.x + step_w, uv.y + step_h)).rgb;
             vec3 xx = t1 + 2.0*t2 + t3 - t7 - 2.0*t8 - t9;
             vec3 yy = t1 - t3 + 2.0*t4 - 2.0*t6 + t7 - t9;
             vec3 rr = sqrt(xx * xx + yy * yy);
             vec3 col = rr * 2.0 * t5;
             if (uv.x < dividerValue) {
                 gl_FragColor = vec4(col, 1.0);
                 if (length(rr)>0.5) {
                     gl_FragColor = vec4(0.5,0.5,1,1);
                 }else{
                     gl_FragColor = qt_Opacity * texture2D(source, uv);
                 }
             } else {
                 gl_FragColor = qt_Opacity * texture2D(source, uv);
             }
         }"
         Component.onCompleted: console.log(step_w, step_h)
     }
 }