#version 130

/* varying vec4 f_color; */
 
/* void main(void) */
/* { */
/*     gl_FragColor = f_color; */
/* } */

#define TWO_PI 6.28318

uniform vec4 ModelColor;
uniform vec3 LightPosition;

uniform sampler2D GlyphTex;
uniform sampler2D RandomTex;

uniform float ColAdjust;
uniform float ScaleFactor;
uniform float Percentage;
uniform float SamplesPerCell;
uniform float RO1;
uniform bool RandomScale;
uniform bool RandomRotate;

in vec2 TexCoord;
in vec3 Normal;
in float LightIntensity;
out vec4 FragColor;

void main() {

     vec4 color = ModelColor;
     vec2 cell = floor(TexCoord);
     vec2 offset = fract(TexCoord);

     for (int i = 1; i <= 0; i++) {
     //    for (int j = 0; j <= 0; j++) {
     int j = 0;
             vec2 cell_t = cell + vec2(i, j);
             vec2 offset_t = offset - vec2(i, j);
             vec2 randomUV = cell.xy * vec2(0.037, 0.119);
             vec4 random = vec4(0.25, 0.25, 0.25, 1);// texture(RandomTex, randomUV);
             vec4 image = texture(GlyphTex, offset_t - random.xy);
             if (image.w > 0.0)
                 color.xyz = image.xyz;



             //         }
     }
     FragColor = color * LightIntensity;
     
     // for (int i = -1; i <= 0; i++) {
     //     for (int j = -1; j <= 0; j++) {
     //         vec2 cell_t = cell + vec2(i, j);
     //         vec2 offset_t = offset - vec2(i, j);
     //         vec2 randomUV = cell_t.xy * vec2(0.037, 0.119);
     //         vec4 random = texture(RandomTex, randomUV);
     //         vec4 image = texture(GlyphTex, offset_t - random.xy);
     //         if (image.w > 0.0)
     //             color = image;
     //     }
     // }
     
     // FragColor = color * LightIntensity;
}

// void main()
// {
//      vec4 color = ModelColor;
//      vec2 cell = floor(TexCoord);
//      vec2 offset = fract(TexCoord);
//      vec2 dPdx = dFdx( TexCoord )/ScaleFactor;
//      vec2 dPdy = dFdy( TexCoord )/ScaleFactor;
    
//     for (int i = -1; i <= int (RandomRotate); i++) {
//         for (int j = -1; j <= int (RandomRotate); j++) {
//             vec2 currentCell = cell + vec2(float(i), float(j));
//             vec2 currentOffset = offset - vec2(float(i), float(j));
//             vec2 randomUV = currentCell * vec2(RO1);
//             for (int k = 0; k < int (SamplesPerCell); k++) {
//                 vec4 random = textureGrad(RandomTex, randomUV, dPdx, dPdy);
//                 randomUV += random.ba;
//                 if (random.r < Percentage) {
//                     vec2 glyphIndex;
//                     mat2 rotator;
//                     vec2 index;
//                     float rotationAngle,cosRot, sinRot;
//                     index.s = floor(random.b * 10.0);
//                     index.t = floor(ColAdjust * 10.0);
//                     if (RandomRotate) {
//                         rotationAngle = TWO_PI * random.g;
//                         cosRot = cos(rotationAngle);
//                         sinRot = sin(rotationAngle);
//                         rotator[0] = vec2(cosRot, sinRot);
//                         rotator[1] = vec2(-sinRot, cosRot);
//                         glyphIndex = -rotator * (currentOffset - random.rg);
//                     } else {
//                         glyphIndex = currentOffset - random.rg;
//                     }

//                     if (RandomScale)
//                         glyphIndex /= vec2(0.5 * random.r + 0.5);
//                     glyphIndex = (clamp(glyphIndex, 0.0, 1.0) + index) * 0.1;
//                     vec4 image = textureGrad(GlyphTex, glyphIndex, dPdx, dPdy);
//                     if (image.r != 1.0)
//                         color.rgb = mix(random.rgb * 0.7, color.rgb, image.r);
//                 }
//             }
//         }
//     }
//     // color = vec4(1,0,0,1);
//     //FragColor = color * LightIntensity;
//     //FragColor = vec4(TexCoord,0,1);
//     FragColor = vec4(1,0,0,1);
// }
