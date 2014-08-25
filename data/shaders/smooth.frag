#version 330
// more or less what http://opengl-tutorial.org teaches
// see https://code.google.com/p/opengl-tutorial-org/source/browse/tutorial08_basic_shading/ for a much nicer version

in vec2 uv;
in vec3 w_position;
in vec3 cam_normal;
in vec3 cam_eye_dir;
in vec3 cam_light_dir;

out vec4 colour;

uniform mat4 VP;
uniform mat4 V;
uniform mat4 P;
uniform float scale;
uniform vec3 offset;
uniform vec3 w_light_pos;
uniform vec4 light_col;
uniform vec4 diffuse;

void main() {

  vec4 ambient   = vec4(0.1 * diffuse.rgb, 1);
  vec4 specular  = vec4(0.5,0.5,0.5,1);

  // distance to light
  float distance_  = length(w_light_pos - w_position);
  float distanceSq = distance_ * distance_;

  vec3 n         = normalize(cam_normal);
  vec3 l         = normalize(cam_light_dir);
  float cosTheta = clamp(dot(n,l),0,1);

  vec3 E         = normalize(cam_eye_dir);
  vec3 R         = reflect(-l, n);
  float cosAlpha = clamp(dot(E, R), 0, 1);

  colour
    = ambient
    + diffuse * light_col * light_col.a * cosTheta/distanceSq
    + specular * light_col * light_col.a * pow(cosAlpha, 5) / distanceSq;
  colour.a = diffuse.a;
}

