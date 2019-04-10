#import "globals.glsl"
#import "buffer.glsl"

#define MAXSIZE $CHUNK3D_SIZE //?^3

layout (location = 0) in vec3 iInstancePos;
layout (location = 1) in vec3 iInstanceFlags;

layout (location = 0) flat out uint index; // flat, because does not change
layout (location = 1) flat out float dummy;

layout(std430) buffer inputBuffer { BuffData instances[]; };

void main() {
  index = gl_VertexID; //+ gl_DrawID * MAXSIZE
  //BuffData data = instances[index];
  //if(data.type >= 0) {} // else discard
  
  // dummy, just to avoid optimization
  if(iInstancePos.x<0){ dummy = iInstancePos.x+iInstanceFlags.x; };
  //gl_Position = vec4(0);
}
