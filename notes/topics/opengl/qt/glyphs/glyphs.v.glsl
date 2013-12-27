#version 130

in vec3 MCVertex;
in vec3 MCNormal;
in vec2 MCTexCoord;
out vec3 Normal;
out vec2 TexCoord;
out float LightIntensity;

uniform float SpecularContribution;
uniform vec3 LightPosition;
uniform float ScaleFactor;
uniform mat4 MVMatrix;
uniform mat4 MVPMatrix;
uniform mat3 NormalMatrix;

void main()
{
    vec3 ecPosition = vec3(MVMatrix * vec4(MCVertex, 1.));
    vec3 norm = normalize(NormalMatrix * MCNormal);
    vec3 lightVec = normalize(LightPosition - ecPosition);

    vec3 reflectVec = reflect(-lightVec, norm);
    vec3 viewVec = normalize(-ecPosition);

    float diffuse = max(dot(lightVec, norm), 0.0);
    float spec = 0.0;
    
    if(diffuse > 0.0) {
        spec = max(dot(reflectVec, viewVec), 0.0);
        spec = pow(spec, 16.0);
    }

    float diffusecontribution = 1.0 - SpecularContribution;
    LightIntensity = min(1, 0.3 + diffusecontribution * diffuse * 2.0 + SpecularContribution * spec);
    
    TexCoord = MCTexCoord.st * ScaleFactor;
    Normal = norm;
    gl_Position = MVPMatrix * vec4(MCVertex, 1.);
}
