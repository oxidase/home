attribute vec3 v_coord;
attribute vec3 v_normal;
uniform mat4 m, v, p;
uniform mat3 m_3x3_inv_transp;
uniform mat4 v_inv;

varying vec4 f_coord;
varying vec4 f_eye;
varying vec3 f_normal;

void main(void)
{
    mat4 mvp = p*v*m;
    f_normal = normalize(m_3x3_inv_transp * v_normal);
    f_coord = mvp * vec4(v_coord, 1.0);
    f_eye = v*m * vec4(v_coord, 1.0);
    gl_Position = f_coord;

}
