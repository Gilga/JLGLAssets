#import "globals.glsl"
#import "landscape.glsl"

#define DIST VOXEL_DIST*0.5

layout(points) in;
layout(triangle_strip, max_vertices=24) out; // 128 is hardware max

layout(location = 0) in Vertex iv[];
layout(location = 0) out Vertex ov;

layout(std430) buffer inputBuffer { BuffData instances[]; };

const vec3 cube[6][4] = {
  {vec3(1,1,1),vec3(1,1,-1),vec3(-1,1,1),vec3(-1,1,-1)},
  {vec3(-1,-1,1),vec3(-1,-1,-1),vec3(1,-1,1),vec3(1,-1,-1)},
  {vec3(1,1,-1),vec3(1,-1,-1),vec3(-1,1,-1),vec3(-1,-1,-1)},
  {vec3(-1,1,1),vec3(-1,-1,1),vec3(1,1,1),vec3(1,-1,1)},
  {vec3(-1,1,1),vec3(-1,1,-1),vec3(-1,-1,1),vec3(-1,-1,-1)},
  {vec3(1,-1,1),vec3(1,-1,-1),vec3(1,1,1),vec3(1,1,-1)}
};
  
void createSide(Vertex v, int side)
{
  //instances[uint(v.flags.x)].height = 1;

  for(int i=0;i<4;++i)
  {
    v.pos          = vec4(cube[side][i]*DIST,1);
    v.normal       = normalize(v.pos);
    v.world_pos    = vec4(v.pos.xyz+v.world_center.xyz,1);
    v.world_normal = normalize(v.world_pos);
    
    ov = v;
    
    gl_Position = iMVP * v.world_pos;
    EmitVertex();
  }
  //EndPrimitive();
}
  
uint sides;

void main()
{
  Vertex v;
  v = iv[0];
    
  if(v.flags.w < 0) return; // discard

  sides = uint(floor(v.flags.z));
  
  if((sides & 0x1) > 0) createSide(v, 4);  // LEFT
  if((sides & 0x2) > 0) createSide(v, 5);  // RIGHT
  if((sides & 0x4) > 0) createSide(v, 0);  // TOP
  if((sides & 0x8) > 0) createSide(v, 1);  // BOTTOM
  if((sides & 0x10) > 0) createSide(v, 2);  // FRONT
  if((sides & 0x20) > 0) createSide(v, 3);  // BACK
}