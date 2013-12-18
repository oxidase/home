attribute vec3 v_coord;
attribute vec3 v_normal;
uniform mat4 m, v, p;
uniform mat3 m_3x3_inv_transp;
uniform mat4 v_inv;
 
varying vec4 f_color;
 
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
    0.0, 1.0, 0.0,
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
    vec4(1.0,  1.0,  1.0, 1.0),
    vec4(1.0,  1.0,  1.0, 1.0),
    16.
);

void DirectionalLight(const in int i,
                      const in vec3 normal,
                      inout vec4 ambient,
                      inout vec4 diffuse,
                      inout vec4 specular)
{
    float nDotVP; // normal . light direction
    float nDotHV; // normal . light half vector
    float pf; // power factor
    
    /* nDotVP = max(0.0, dot(normal, normalize(vec3(LightSource[i].position)))); */
    /* nDotHV = max(0.0, dot(normal, vec3(LightSource[i].halfVector))); */

    /* if (nDotVP == 0.0) */
    /*     pf = 0.0; */
    /* else */
    /*     pf = pow(nDotHV, FrontMaterial.shininess); */
    /* ambient += LightSource[i].ambient; */
    /* diffuse += LightSource[i].diffuse * nDotVP; */
    /* specular += LightSource[i].specular * pf; */
}


void main(void)
{
    vec4 v_coord4 = vec4(v_coord, 1.0);
    mat4 mvp = p*v*m;
    vec3 normalDirection = normalize(m_3x3_inv_transp * v_normal);
    vec3 viewDirection = normalize(vec3(v_inv * vec4(0.0, 0.0, 0.0, 1.0) - m * v_coord4));
    vec3 lightDirection;
 
    if (light0.position.w == 0.0) {
        lightDirection = normalize(vec3(light0.position));
    } else {
        vec3 vertexToLightSource = vec3(light0.position - m * v_coord4);
        lightDirection = normalize(vertexToLightSource);
    }
 
    float levelOfLighting = max(0.0, dot(normalDirection, lightDirection));

    float attenuation = 1.;
    
    vec3 specularReflection = vec3(0.0, 0.0, 0.0);
    if (levelOfLighting > 0.0) {
        specularReflection = attenuation * vec3(light0.specular) * vec3(material0.specular)
            * pow(max(0.0, dot(reflect(-lightDirection, normalDirection), viewDirection)), material0.shininess);
    }
    
    gl_Position = mvp * v_coord4;

    f_color = light0.ambient + light0.diffuse*levelOfLighting + vec4(specularReflection, 1.0);
}
