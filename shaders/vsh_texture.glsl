#import "globals.glsl"

uniform float iTexIndex = 0;

layout (location = 0) in vec3 iVertex;
layout (location = 0) out Vertex vertex;

void main() {
  bool dummy = false; if (iVertex.x == 0) dummy = false; // disable out optimization
  Vertex v = _Vertex();
  vec4 outpos = vec4(0,0,0,0);
  v.flags = vec4(0,(iTexIndex-1),0,0);

  if(v.flags.x >= 0) {
    v = _preset(iVertex, iPosition);
    outpos = iMVP*v.world_pos;
    if(v.flags.y >= 0) { v.uvs.zw = getTexUV(v.flags.y); }
  } // else discard
  
  vertex = v;
  gl_Position = outpos;
}