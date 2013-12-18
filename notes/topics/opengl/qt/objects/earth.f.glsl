varying float levelOfLighting;
varying vec4 texCoords;
uniform sampler2D texture_day;
uniform sampler2D texture_night;
 
void main(void)
{
    vec2 longitudeLatitude = vec2((atan(texCoords.y, texCoords.x) / 3.1415926 + 1.0) * 0.5,
                                  (asin(texCoords.z) / 3.1415926 + 0.5));
 
    vec4 daytimeColor = texture2D(texture_day, longitudeLatitude);
    vec4 nighttimeColor = texture2D(texture_night, longitudeLatitude);
    
    gl_FragColor = mix(nighttimeColor, daytimeColor, levelOfLighting);
}
