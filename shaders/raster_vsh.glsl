#import "globals.glsl"
#import "buffer.glsl"

#define MAXSIZE $CHUNK3D_SIZE //?^3

layout (location = 0) in vec3 iInstancePos;
layout (location = 1) in vec3 iInstanceFlags;

layout (location = 0) out Vertex vertex;

layout(std430) buffer inputBuffer { BuffData instances[]; };

void main() {
  Vertex v = _Vertex();
  uint index = gl_VertexID;
  
  BuffData data = instances[index];
  //instances[index].height = 0;

  vec3 pos = getPos(data) + iCenter + iPosition;

  v.flags = vec4(getType(data)*0+index,0,getSides(data),getLevel(data)); //vec4(data.type*0+index,0,data.sides,data.height);
  
  if(v.flags.w >= 0) {
    v.world_center  = vec4(pos,1);
    v.size = vec4(0,0,iInstancePos.x,iInstanceFlags.x);
    vertex = v;
  } // else discard
 
  //vertex = v;
  //gl_Position = vec4(0,0,0,0);
}
