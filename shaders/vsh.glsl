#import "globals.glsl"

layout (location = 0) in vec3 iVertex;

void main() {
  bool dummy = false; if (iVertex.x == 0) dummy = false; // disable out optimization
  gl_Position = iMVP*vec4(iVertex+iPosition,1);
}