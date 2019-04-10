#import "globals.glsl"
#import "landscape.glsl"

#define DIST VOXEL_DIST*0.5
#define INVERSE_DIST 1/VOXEL_DIST

layout(points) in;
layout(triangle_strip, max_vertices=85) out; // 128 is hardware max
//triangle_strip, line_strip

layout(location = 0) flat in uint points[];

layout(location = 0) flat out uint index;
layout(location = 2) out float wave;
layout(location = 3) out vec3 pos;
layout(location = 4) out vec3 normal;

layout(std430) buffer inputBuffer { BuffData instances[]; };

const vec3 cube[6][4] = {
  {vec3(1,1,1),vec3(1,1,-1),vec3(-1,1,1),vec3(-1,1,-1)},
  {vec3(-1,-1,1),vec3(-1,-1,-1),vec3(1,-1,1),vec3(1,-1,-1)},
  {vec3(1,1,-1),vec3(1,-1,-1),vec3(-1,1,-1),vec3(-1,-1,-1)},
  {vec3(-1,1,1),vec3(-1,-1,1),vec3(1,1,1),vec3(1,-1,1)},
  {vec3(-1,1,1),vec3(-1,1,-1),vec3(-1,-1,1),vec3(-1,-1,-1)},
  {vec3(1,-1,1),vec3(1,-1,-1),vec3(1,1,1),vec3(1,1,-1)}
  };
  
const vec3 normals[6] = {vec3(0,1,0), vec3(0,-1,0), vec3(0,0,-1), vec3(0,0,1), vec3(-1,0,0), vec3(1,0,0)};

vec3 CalculateSurfaceNormal(vec3 Triangle[4])
{
  vec3 normal = vec3(0);

	vec3 U = Triangle[1] - Triangle[0];
	vec3 V = Triangle[2] - Triangle[0];
  
	normal.x = (U.y * V.z) - (U.z * V.y);
	normal.y = (U.z * V.x) - (U.x * V.z);
	normal.z = (U.x * V.y) - (U.y * V.x);

	return normal;
}
  
void createPoint(vec3 center)
{
  gl_Position = iMVP * vec4(center,1);
  EmitVertex();
  EndPrimitive();
}

const bool AnimatedWater = false;

// math
mat3 fromEuler(vec3 ang) {
	vec2 a1 = vec2(sin(ang.x),cos(ang.x));
    vec2 a2 = vec2(sin(ang.y),cos(ang.y));
    vec2 a3 = vec2(sin(ang.z),cos(ang.z));
    mat3 m;
    m[0] = vec3(a1.y*a3.y+a1.x*a2.x*a3.x,a1.y*a2.x*a3.x+a3.y*a1.x,-a2.y*a3.x);
	m[1] = vec3(-a2.y*a1.x,a1.y*a2.y,a2.x);
	m[2] = vec3(a3.y*a1.x*a2.x+a1.y*a3.x,a1.x*a3.x-a1.y*a3.y*a2.x,a2.y*a3.y);
	return m;
}

void createSide(int side, vec3 center, float lod, bool isWater)
{
  vec3 CameraRight_worldspace = vec3(iMVP[0][0], iMVP[1][0], iMVP[2][0]);
  vec3 CameraUp_worldspace = vec3(iMVP[0][1], iMVP[1][1], iMVP[2][1]);
  bool water = AnimatedWater ? isWater : false;
  vec3 add;
  vec3 world_pos;
  
  vec3 local_center = center*INVERSE_DIST;
  
        
  // ray
  float time = iTime * 0.3; // + iMouse.x*0.01;
  vec3 ang = vec3(sin(time*3.0)*0.1,sin(time)*0.2+0.3,time);    
  vec3 ori = vec3(0,0,time*5.0);
    
  for(int i=0;i<4;++i)
  {
    pos = cube[side][i];
    //normal = normalize(pos);
    normal = normals[side];
    
    add = vec3(0);
    //if((side == 2 && (i == 1 || i == 3)) || (side == 3 && (i == 1 || i == 3)) || (side == 4 && (i == 2 || i == 3)) || (side == 5 && (i == 0 || i == 1))) pos.y *= 1+abs(1-v.flags.w)*300;
    if(water) {
      vec2 uv = vec2((pos.x*0.5+local_center.x), (pos.z*0.5+local_center.z));
      vec3 dir = normalize(vec3(uv.xy,-0.0)); dir.z += length(uv) * -0.15;
      wave = (map(ori + dir) - 3)*3;
      add.y -= (side == 0 ? 3 : 1) + wave;
      wave= 0.5+sin(wave)*0.5;
     }
    
    //vec3 vpos = (CameraRight_worldspace * pos.x + CameraUp_worldspace * pos.y * 0.6);

    world_pos    = (pos+add)*DIST*lod+center;
    gl_Position = iMVP * vec4(world_pos,1);

    EmitVertex();
  }
  EndPrimitive();
}
  
MapData flags;

void main()
{
  index = points[0];
  BuffData data = instances[index];
  
  uint type = getType(data);

  if(type < 0) return; // discard
  
  vec3 pos = getPos(data);
  pos += iCenter + iPosition;
  float lod = 1; //getLOD(data);

  //if(!is_visible(pos)) return; // discard
  
  uint sides = getSides(data);
  
  bool isWater = type == 16;
  bool isCamAboveSurface = -iCamPos.y >= (pos.y-1);
  sides = uint(isWater ? (isCamAboveSurface ? 4 : 8) : sides);
  
  //for(uint i=0; i<1; i++) {
    if((sides & 0x1) > 0) createSide(4, pos, lod, isWater);  // LEFT
    if((sides & 0x2) > 0) createSide(5, pos, lod, isWater);  // RIGHT
    if((sides & 0x4) > 0) createSide(0, pos, lod, isWater);  // TOP
    if((sides & 0x8) > 0) createSide(1, pos, lod, isWater);  // BOTTOM
    if((sides & 0x10) > 0) createSide(2, pos, lod, isWater);  // FRONT
    if((sides & 0x20) > 0) createSide(3, pos, lod, isWater);  // BACK
    //pos.y += 100;
  //}
}

/*
void main()
{
  int i;
  int len = gl_in.length();

  for(i=0; i<len; ++i)
  {
    if (v.texindex < 0) continue;
    ov = v;
    gl_Position = iMVP * gl_in[i].gl_Position;
    EmitVertex();
  }
  EndPrimitive();
}
*/