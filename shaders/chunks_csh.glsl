#import "globals.glsl"
#import "landscape.glsl"

layout (local_size_x = $CHUNK_SIZE) in; //, local_size_y = 4, local_size_z = 4

// Can also do atomic operations on an SSBO.
// instanceCount in indirect draw buffer is found at offset = 4.
layout(binding = 0, offset = 0) uniform atomic_uint instanceCount;
layout(binding = 1, offset = 0) uniform atomic_uint dispatch;

layout(std430, binding = 0) writeonly buffer Output {
  Data data[];
} outputset;

void main() {
  uint ident  = gl_GlobalInvocationID.x;
  
  if(ident == 0) atomicCounterExchange(instanceCount, 0);
  
  vec3 index = getIndex(ident);
  MapData flags = getTypeSide2(index);
  
  //if (flags.x <= 0 || flags.y <= 0) return;
  //if(index.y != 0) return;

  uint unique  = atomicCounterIncrement(instanceCount);
  outputset.data[unique] = Data(float[3](index.x,index.y,index.z),flags.type,flags.sides,flags.height);
  atomicCounterExchange(dispatch, uint(round(float(unique)/gl_WorkGroupSize.x)));
}