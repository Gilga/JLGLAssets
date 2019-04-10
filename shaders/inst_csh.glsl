#import "globals.glsl"

layout (local_size_x = $CHUNK_SIZE) in; //, local_size_y = 4, local_size_z = 4

struct Data {
  float[3] pos; //4*3
  float type;
  float sides;
  float height;
};

layout(binding = 0, offset = 0) uniform atomic_uint LIMIT;
layout(binding = 1, offset = 0) uniform atomic_uint instanceCount;

layout(std430, binding = 0) readonly buffer Input {
  Data data[];
} inputset;

layout(std430, binding = 1) writeonly buffer Output {
  Data data[];
} outputset;

void main() {
  uint ident  = gl_GlobalInvocationID.x;
  
  if(ident == 0) atomicCounterExchange(instanceCount, 0);
  if(ident >= atomicCounter(LIMIT)) return;
  
  Data data = inputset.data[ident];

  barrier();
  uint unique  = atomicCounterIncrement(instanceCount);
  outputset.data[unique] = Data(data.pos,data.type,data.sides,data.height);
}