// more or less what http://opengl-tutorial.org teaches
// see https://code.google.com/p/opengl-tutorial-org/source/browse/tutorial08_basic_shading/ for a much nicer version

in vec3 position;
in vec2 uvs;
in vec3 normal;

varying out vec2 uv;
varying out vec3 w_position;
varying out vec3 cam_normal;
varying out vec3 cam_eye_dir;
varying out vec3 cam_light_dir;

uniform mat4 VP;
uniform mat4 V;
uniform mat4 P;
uniform float scale;
uniform vec3 w_offset;
uniform vec3 w_light_pos;
uniform vec4 light_col;
uniform vec4 diffuse;

mat4 M = mat4
  (scale, 0.0, 0.0, 0.0,
   0.0, scale, 0.0, 0.0,
   0.0, 0.0, scale, 0.0,
   w_offset.x, w_offset.y, w_offset.z, 1.0);

mat4 MVP = VP * M;
  
void main() {
  gl_Position   = MVP * vec4(position, 1);
  w_position    = gl_Position.xyz;
  uv            = uvs;

  vec3 cam_vert_pos = (V * M * vec4(position, 1)).xyz;
  cam_eye_dir   = vec3(0,0,0) - cam_vert_pos;

  vec3 cam_light_pos = (V * vec4(w_light_pos, 1)).xyz;
  cam_light_dir = cam_light_pos + cam_eye_dir;

  cam_normal    = (V * M * vec4(normal,0)).xyz;
}
