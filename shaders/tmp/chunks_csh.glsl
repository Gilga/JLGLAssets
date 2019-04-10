#version 460
//#extension GL_ARB_shader_atomic_counter_ops : require
#define PI 3.14159265359
#define HALF (0.5 + 1/PI)
#define RADIUS (1 - 0.01/PI)
#define UVHALF 0.57735
#define UVFULL (1-0.000001)

const mat4 IdentityMatrix = mat4(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1);

uniform mat4 iMVP = IdentityMatrix;
uniform mat4 iProj = IdentityMatrix;
uniform mat4 iView = IdentityMatrix;
uniform mat4 iModel = IdentityMatrix;

uniform vec3 iPosition = vec3(0);
uniform vec3 iCamPos = vec3(0);
uniform vec3 iCamAng = vec3(0);
uniform vec3 iCenter = vec3(0);

uniform bool frustum = false;
uniform vec3 frustum_dirs[6] = vec3[6](vec3(0,0,0),vec3(0,0,0),vec3(0,0,0),vec3(0,0,0),vec3(0,0,0),vec3(0,0,0));
uniform float frustum_dists[6] = float[6](0,0,0,0,0,0);

uniform vec2 iResolution = vec2(0,0);
uniform float iTime = 1;
uniform bool iUseLight = true;
uniform bool iUseTexture = true;

struct Vertex
{
	vec4 pos;
	vec4 normal;
	vec4 color;
  vec4 flags;
  vec4 uvs;
  vec4 world_center;
  vec4 world_pos;
	vec4 world_normal;
};

Vertex _Vertex() { return Vertex(vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0)); }

vec2 getUV(vec3 vertex)
{
	int i = 1;

	bool bag = i==0;
	bool cube = i==1;
	
	float dist = bag?0:cube?UVFULL:UVHALF;
	
	vec3 normal = normalize(vertex);
	normal = clamp(cube?vertex:normal,-1,1);
	
	float x = normal.x;
	float y = normal.y;
	float z = normal.z;
		
	vec2 uv = vec2(0);
	
	bool found = false;
	
	if (bag)
	{
		uv+=vec2(x,y);
		found=true;
	}
	else
	{
		bool fb = abs(z)>=dist;
		bool rl = abs(x)>=dist;
		bool ud = abs(y)>=dist;
		
		float u = x;
		float v = y;
		
		if(fb) u = (z>0?1:-1)*x;
		if(rl) u = (x>0?1:-1)*-z;
		if(ud) {u = x; v = -z;}
		
		uv+=vec2(u,v);
		found=true;
	}
	if(!found) return vec2(-1);
	
	uv = (1+uv)*0.5;
	uv = clamp(uv,0,1);
	
	return uv;
}

vec4 getVertexColor(vec3 pos, vec3 normal, float time)
{
  if(false) // pos.z != 0 skip planes
  {
    float len = dot(length(pos),length(normal)); // skip cubes and spheres
    if(len < RADIUS) pos = (pos + normal) * HALF; // model
  }
  
  vec3 color1 = (1-normal)*0.5;
  vec3 color2 = (1+normal)*0.5;
  vec3 color = mix(color1,color2, sin(time));
  
  return vec4(color,1.0);
}

vec2 getTexUV(float texindex){
  int tx = 0, ty = 0;
  for(int i=0; i<floor(texindex); ++i) {
    tx++; if(tx>=4) {
      tx=0; ty++;
      if(ty>=4) ty=0;
    }
  }
  return vec2(tx,ty);
}

Vertex _preset(vec3 pos, vec3 world){
  Vertex v = _Vertex();

  v.pos          = vec4(pos,1);
  v.normal       = normalize(v.pos);
  v.uvs          = vec4(0);
  v.color        = getVertexColor(v.pos.xyz, v.normal.xyz, 1);
  // flags
  v.world_center = vec4(world,0);
  v.world_pos    = vec4(v.pos.xyz+v.world_center.xyz,1);
  v.world_normal = normalize(v.world_pos);
  
  return v;
}

Vertex preset(vec3 pos){
  return _preset(pos, vec3(0,0,0));
}

float getDistance(float dist, vec3 normal, vec3 pos){
  return dist + dot(normal, pos);
}

int checkSphere(vec3 pos, float radius){
  int result = 1;
  float dist = 0;
  
  for (int i=0; i<6; i++){
    dist = getDistance(frustum_dists[i], frustum_dirs[i], pos);
		if (dist < -radius) result = -1;
		else if(dist <= radius && result != -1) result = 0;
	}

	return result;
}
#define COLSIZE 128 //?^1
#define ROWSIZE 16384 //?^2
#define MAXSIZE 2097152 //?^3
#define LAST MAXSIZE-1

#define COLISIZE 1.0/COLSIZE
#define ROWISIZE 1.0/ROWSIZE
#define MAXISIZE 1.0/MAXSIZE

#define SEED 43758.5453123
#define NUM_OCTAVES 5

float DIST = 2;
float STARTDIST = (COLSIZE*DIST) / 2.0;
vec3 START = vec3(-1,-1,1)*STARTDIST;

//float = 1 * 4 bytes 
// Never use a vec3 in a UBO or SSBO.
// https://stackoverflow.com/questions/38172696/should-i-ever-use-a-vec3-inside-of-a-uniform-buffer-or-shader-storage-buffer-o
struct Data {
  float[3] pos; //4*3
  float type;
  float sides;
  //float height;
};

struct MapData {
  float type;
  float sides;
  float height;
};

vec3 translate(vec3 index) { return START+index*vec3(1,1,-1)*DIST; }

vec3 getIndex(uint index) {
  float i = index;
  float y = floor(i*ROWISIZE);
  float height = y*ROWSIZE;
  float z = floor((i-height)*COLISIZE);
  float x = i-height-z*COLSIZE;
  return vec3(x,y,z);
}

float random (in float n) { return fract(sin(n)*43758.5453123); }
float random (in vec2 _st) { return fract(sin(dot(_st.xy,vec2(12.9898,78.233)))*43758.5453123); }

float noise (in vec2 _st) {
    vec2 i = floor(_st);
    vec2 f = fract(_st);

    // Four corners in 2D of a tile
    float a = random(i);
    float b = random(i + vec2(1.0, 0.0));
    float c = random(i + vec2(0.0, 1.0));
    float d = random(i + vec2(1.0, 1.0));

    vec2 u = f * f * (3.0 - 2.0 * f);

    return mix(a, b, u.x) +
            (c - a)* u.y * (1.0 - u.x) +
            (d - b) * u.x * u.y;
}

float fbm (in vec2 _st) {
    float v = 0.0;
    float a = 0.5;
    vec2 shift = vec2(100.0);
    // Rotate to reduce axial bias
    mat2 rot = mat2(cos(0.5), sin(0.5),
                    -sin(0.5), cos(0.50));
    for (int i = 0; i < NUM_OCTAVES; ++i) {
        v += a * noise(_st);
        _st = rot * _st * 2.0 + shift;
        a *= 0.5;
    }
    return v;
}

vec2 texSize = vec2(1024,1024);

float getLandscapeHeight(vec2 uv){
 return clamp(fbm(uv*3.0)*1,0.0,1.0);// texture(srcTex, uv * texSize).r;
}

MapData getTypeSide2(vec3 index){
  float S = COLISIZE;
  float y = index.y * S;
  vec2 uv = index.xz * S;

  //if(uv.x < 0 || uv.x >= 1 || uv.y < 0 || uv.y >= 1) return vec2(-1,0);

  vec4 next = vec4(uv.x-S,uv.x+S,uv.y+S,uv.y-S);
  vec2 nextTB = vec2(y+S,y-S);
  
  bool left = true; //next.x >= 0 && next.x < 1;
  bool right = true; //next.y >= 0 && next.y < 1;
  bool forward = true; //next.z >= 0 && next.z < 1;
  bool back = true; //next.w >= 0 && next.w < 1;
  bool top = true; //nextTB.x >= 0 && nextTB.x < 1;
  bool bottom = true; //nextTB.y >= 0 && nextTB.y < 1;
  
  float height = getLandscapeHeight(uv);
  uint sides=0;
  
  if(left) left = y <= getLandscapeHeight(vec2(next.x,uv.y));
  if(right) right = y <= getLandscapeHeight(vec2(next.y,uv.y));
  if(forward) forward = y <= getLandscapeHeight(vec2(uv.x,next.z));
  if(back) back = y <= getLandscapeHeight(vec2(uv.x,next.w));
  if(top) top = nextTB.x <= height;
  if(bottom) bottom = nextTB.y <= height;
  
  if(left && right && forward && back && top && bottom) return MapData(-1,0,-1);
  
  if(!left) sides |= (0x1 << 0); // LEFT
  if(!right) sides |= (0x1 << 1); // RIGHT
  if(!top) sides |= (0x1 << 2); // TOP
  if(!bottom) sides |= (0x1 << 3); // BOTTOM
  if(!forward) sides |= (0x1 << 4); // FRONT
  if(!back) sides |= (0x1 << 5); // BACK
  
  //float level_air = height * 0.99;
  float level_grass = height * 0.95;
  float level_dirt = height * 0.9;
  float level_stonebricks = height * 0.6;
  float level_stone = height * 0.5;
  float level_lava = height * fbm(index.xz*0.002)*1.1;
  
  float typ = 0;

  if (y <= height) {
    //if (y >= level_air) typ = 0; // air or nothing
    if (y <= level_lava) typ = 15; //lava
    else if (y <= level_stone) typ = 4; //stone
    else if (y <= level_stonebricks) typ = 5; //stonebricks
    else if (y <= level_dirt) typ = 1; //dirt
    else typ = 2; //grass
    
    float g = fbm(index.xz);
    if(g >= 0.8 && g <= 1.0) typ = 5;
    if(g >= 0.1 && g <= 0.2) typ = 4;
    if(y > level_dirt && g >= 0.3 && g <= 0.5) typ = 9;
  }
  
  if (y >= 0.33 && y <= 0.34 && typ == 0) {
    typ = 16;
    sides=4;
  }
  
  float h = y/height;
  if (typ == 0 || sides == 0 || h>1) { h=-1; typ=0; sides=0; }
  
  return MapData(1,sides,h);
}

vec3 getTypeSide(vec3 index){
  float S = COLISIZE;
  float y = index.y * S;
  vec2 uv = index.xz * S;
  
  float height = getLandscapeHeight(uv);
  y = 1;

  //if(uv.x < 0 || uv.x >= 1 || uv.y < 0 || uv.y >= 1) return vec2(-1,0);

  vec4 next = vec4(uv.x-S,uv.x+S,uv.y+S,uv.y-S);
  vec2 nextTB = vec2(y+S,y-S);
  
  bool left = true; //next.x >= 0 && next.x < 1;
  bool right = true; //next.y >= 0 && next.y < 1;
  bool forward = true; //next.z >= 0 && next.z < 1;
  bool back = true; //next.w >= 0 && next.w < 1;
  bool top = true; //nextTB.x >= 0 && nextTB.x < 1;
  bool bottom = true; //nextTB.y >= 0 && nextTB.y < 1;
  
  uint sides=0;
  float typ = 0;
  
  if(left) left = y <= getLandscapeHeight(vec2(next.x,uv.y));
  if(right) right = y <= getLandscapeHeight(vec2(next.y,uv.y));
  if(forward) forward = y <= getLandscapeHeight(vec2(uv.x,next.z));
  if(back) back = y <= getLandscapeHeight(vec2(uv.x,next.w));
  if(top) top = nextTB.x <= height;
  if(bottom) bottom = nextTB.y <= height;
  
  if(left && right && forward && back && top && bottom) return vec3(-1,0,0);
  
  if(!left) sides |= (0x1 << 0); // LEFT
  if(!right) sides |= (0x1 << 1); // RIGHT
  if(!top) sides |= (0x1 << 2); // TOP
  if(!bottom) sides |= (0x1 << 3); // BOTTOM
  if(!forward) sides |= (0x1 << 4); // FRONT
  if(!back) sides |= (0x1 << 5); // BACK
  
  //float level_air = height * 0.99;
  float level_grass = height * 0.95;
  float level_dirt = height * 0.9;
  float level_stonebricks = height * 0.6;
  float level_stone = height * 0.5;
  float level_lava = height * fbm(index.xz*0.002)*1.1;

  //if (y <= height) {
    //if (y >= level_air) typ = 0; // air or nothing
    if (y <= level_lava) typ = 15; //lava
    else if (y <= level_stone) typ = 4; //stone
    else if (y <= level_stonebricks) typ = 5; //stonebricks
    else if (y <= level_dirt) typ = 1; //dirt
    else typ = 2; //grass
    
    float g = fbm(index.xz);
    if(g >= 0.8 && g <= 1.0) typ = 5;
    if(g >= 0.1 && g <= 0.2) typ = 4;
    if(y > level_dirt && g >= 0.3 && g <= 0.5) typ = 9;
  //}
  
  if (y >= 0.33 && y <= 0.34 && typ == 0) {
    typ = 16;
    sides=4;
  }
  
  return vec3(typ,sides,height*128);
}

void synchronize()
{
    // Ensure that memory accesses to shared variables complete.
    memoryBarrierBuffer();
    memoryBarrierShared();
    groupMemoryBarrier();
    // Every thread in work group must reach this barrier before any other thread can continue.
    barrier();
}

layout (local_size_x = 128) in; //, local_size_y = 4, local_size_z = 4

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