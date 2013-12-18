uniform mat4 m, v, p;
uniform mat4 v_inv;
 
varying vec4 f_coord;
varying vec4 f_eye;
varying vec3 f_normal;
 
struct LightSource {
    vec4 position;
    vec4 ambient;
    vec4 diffuse;
    vec4 specular;
    float constantAttenuation, linearAttenuation, quadraticAttenuation;
    float spotCutoff, spotExponent;
    vec3 spotDirection;
};
LightSource light0 = LightSource(
    vec4(1.0,  2.0,  1.0, 0.0),
    vec4(0.3,  0.3,  0.3, 1.0),
    vec4(1.0,  1.0,  1.0, 1.0),
    vec4(1.0,  1.0,  1.0, 1.0),
    1.0, 0.001, 0.0,
    180.0, 0.0,
    vec3(0.0, 0.0, 0.0)
);

struct Material {
    vec4 ambient;
    vec4 diffuse;
    vec4 specular;
    float shininess;
};
Material material0 = Material(
    vec4(0.3,  0.3,  0.3, 1.0),
    vec4(0.0,  0.0,  1.0, 1.0),
    vec4(0.0,  1.0,  0.0, 1.0),
    16.
);

void main(void)
{
    vec4 v_coord4 = f_coord;
    mat4 mvp = p*v*m;
    vec3 normalDirection = f_normal;
    vec3 viewDirection = normalize(vec3(v_inv * vec4(0.0, 0.0, 0.0, 1.0) - m * v_coord4));
    vec3 lightDirection;
 
    if (light0.position.w == 0.0) {
        lightDirection = normalize(vec3(light0.position));
    } else {
        vec3 vertexToLightSource = vec3(light0.position - m * v_coord4);
        lightDirection = normalize(vertexToLightSource);
    }
 
    float levelOfLighting = max(0.0, dot(normalDirection, lightDirection));

    float d = length(f_coord - light0.position);
    float attenuation =  1.0 / (light0.constantAttenuation +
                                light0.linearAttenuation * d +
                                light0.quadraticAttenuation * d * d);


    vec4 diffuseLightning = attenuation*light0.diffuse*material0.diffuse*levelOfLighting;
        
    vec3 specularReflection = vec3(0.0, 0.0, 0.0);
    if (levelOfLighting > 0.0) {
        specularReflection = attenuation * vec3(light0.specular) * vec3(material0.specular)
            * pow(max(0.0, dot(reflect(-lightDirection, normalDirection), viewDirection)), material0.shininess);
    }

    vec4 color = light0.ambient + diffuseLightning + vec4(specularReflection, 1.0);
    
    float fog_coord = abs(f_eye.z);
    float fog = clamp((100. - fog_coord) * .1, 0., 1.);
    
    gl_FragColor = mix(vec4(0.2,0.2,0.9,1), color, fog);
}
