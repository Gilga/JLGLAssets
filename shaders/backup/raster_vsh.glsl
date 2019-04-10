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
uniform vec4 frustum_center = vec4(0);
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
  vec4 size;
  vec4 flags;
  vec4 uvs;
  vec4 world_center;
  vec4 world_pos;
	vec4 world_normal;
};

Vertex _Vertex() { return Vertex(vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0),vec4(0)); }

vec2 getUV(vec3 vertex)
{
	int i = 1;

	bool bag = i==0;
	bool cube = i==1;
	
	float dist = bag?0:cube?UVFULL:UVHALF;
	
	vec3 normal = clamp(cube?vertex:normalize(vertex),-1,1);
	
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

vec4 getVertexColor(vec3 normal, float time)
{
  vec3 color1 = (1-normal)*0.5;
  vec3 color2 = (1+normal)*0.5;
  vec3 color = mix(color1,color2, sin(time));
  
  return vec4(color,1.0);
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

vec3 rgb2hsv(vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

float sinA(float v) { return 0.5+sin(v)*0.5; }
float cosA(float v) { return 0.5+cos(v)*0.5; }
precision highp int;
precision highp float;

#define COLSIZE 128 //?^1
#define ROWSIZE 16384 //?^2
#define MAXSIZE 2097152 //?^3
#define LAST MAXSIZE-1
#define DISPATCHSIZE ROWSIZE

#define COLISIZE 1.0/COLSIZE
#define ROWISIZE 1.0/ROWSIZE
#define MAXISIZE 1.0/MAXSIZE

#define VISIBLE_FLAG 0x1

float VOXEL_DIST = 1;
float STARTDIST = (COLSIZE*VOXEL_DIST) / 2.0;

const vec3[] CHUNK_POSITIONS = vec3[](
  vec3(0,0,0),
  vec3(1,0,0), vec3(-1,0,0), vec3(0,0,1), vec3(0,0,-1), vec3(-1,0,-1), vec3(1,0,1), vec3(-1,0,1), vec3(1,0,-1),
  vec3(2,0,0), vec3(-2,0,0), vec3(0,0,2), vec3(0,0,-2), vec3(-2,0,-2), vec3(2,0,2), vec3(-2,0,2), vec3(2,0,-2),
  vec3(2,0,1), vec3(2,0,-1), vec3(-2,0,1), vec3(-2,0,-1), vec3(1,0,2), vec3(-1,0,2), vec3(1,0,-2), vec3(-1,0,-2),
  vec3(3,0,0), vec3(-3,0,0), vec3(0,0,3), vec3(0,0,-3), vec3(-3,0,-3), vec3(3,0,3), vec3(-3,0,3), vec3(3,0,-3),
  vec3(3,0,2), vec3(3,0,-2), vec3(-3,0,2), vec3(-3,0,-2), vec3(2,0,3), vec3(-2,0,3), vec3(2,0,-3), vec3(-2,0,-3),
  vec3(3,0,1), vec3(3,0,-1), vec3(-3,0,1), vec3(-3,0,-1), vec3(1,0,3), vec3(-1,0,3), vec3(1,0,-3), vec3(-1,0,-3),
  vec3(4,0,0), vec3(-4,0,0), vec3(0,0,4), vec3(0,0,-4), vec3(-4,0,-4), vec3(4,0,4), vec3(-4,0,4), vec3(4,0,-4),
  vec3(4,0,3), vec3(4,0,-3), vec3(-4,0,3), vec3(-4,0,-3), vec3(3,0,4), vec3(-3,0,4), vec3(3,0,-4), vec3(-3,0,-4),
  vec3(4,0,2), vec3(4,0,-2), vec3(-4,0,2), vec3(-4,0,-2), vec3(2,0,4), vec3(-2,0,4), vec3(2,0,-4), vec3(-2,0,-4),
  vec3(4,0,1), vec3(4,0,-1), vec3(-4,0,1), vec3(-4,0,-1), vec3(1,0,4), vec3(-1,0,4), vec3(1,0,-4), vec3(-1,0,-4),
  vec3(5,0,0), vec3(-5,0,0), vec3(0,0,5), vec3(0,0,-5), vec3(-5,0,-5), vec3(5,0,5), vec3(-5,0,5), vec3(5,0,-5),
  vec3(5,0,4), vec3(5,0,-4), vec3(-5,0,4), vec3(-5,0,-4), vec3(4,0,5), vec3(-4,0,5), vec3(4,0,-5), vec3(-4,0,-5),
  vec3(5,0,3), vec3(5,0,-3), vec3(-5,0,3), vec3(-5,0,-3), vec3(3,0,5), vec3(-3,0,5), vec3(3,0,-5), vec3(-3,0,-5),
  vec3(5,0,2), vec3(5,0,-2), vec3(-5,0,2), vec3(-5,0,-2), vec3(2,0,5), vec3(-2,0,5), vec3(2,0,-5), vec3(-2,0,-5),
  vec3(5,0,1), vec3(5,0,-1), vec3(-5,0,1), vec3(-5,0,-1), vec3(1,0,5), vec3(-1,0,5), vec3(1,0,-5), vec3(-1,0,-5)
);

vec3 translate(vec3 index) { return STARTDIST*vec3(-1,-1,1)+index*vec3(1,1,-1)*VOXEL_DIST; }
vec3 translate(vec3 index, float dist) { return (((COLSIZE*VOXEL_DIST)/2.0)*vec3(-1,-1,1)+index*vec3(1,1,-1)*VOXEL_DIST); }

vec3 getIndexPos(float index)
{
  float i = index;
  float y = floor(i*ROWISIZE);
  float height = y*ROWSIZE;
  float z = floor((i-height)*COLISIZE);
  float x = i-height-z*COLSIZE;
  return vec3(x,y,z);
}

vec3 getIndex2DPos(float index)
{
  float i = index;
  float y = floor(i*COLSIZE);
  float x = i-y*COLSIZE;
  return vec3(x,0,y);
}

float getIndex(vec3 indexPos) { return indexPos.y*ROWSIZE+indexPos.z*COLSIZE+indexPos.x; }

//float = 1 * 4 bytes 
// Never use a vec3 in a UBO or SSBO.
// https://stackoverflow.com/questions/38172696/should-i-ever-use-a-vec3-inside-of-a-uniform-buffer-or-shader-storage-buffer-o
struct BuffData {
  uint index;
  uint flags;
};

BuffData createBuffData() { return BuffData(0,0); }
BuffData createBuffData(uint ident, uint chunk, uint flags) {
  vec3 index = getIndexPos(ident);
  return BuffData((chunk << 24) | (uint(index.y) << 16) | (uint(index.z) << 8) | uint(index.x), flags);
}
BuffData createBuffData(vec3 index, uint chunk, uint flags) {
  return BuffData((chunk << 24) | (uint(index.y) << 16) | (uint(index.z) << 8) | uint(index.x), flags);
}

float getLOD(BuffData data) { return 1; }
uint getFlags(BuffData data) { return data.flags; }
void setFlags(inout BuffData data, uint flags) { data.flags = flags; }

vec3 getIndex(BuffData data) {
  uint chunk = (data.index >> 24) & 0xFF;
  vec3 index = vec3(data.index & 0xFF, ((data.index >> 16) & 0xFF), (data.index >> 8) & 0xFF);
  return index * getLOD(data) + CHUNK_POSITIONS[chunk]*COLSIZE;
}

vec3 getPos(BuffData data) { return translate(getIndex(data), getLOD(data)); }
//void setPos(inout BuffData data, vec3 pos) { data.pos = pos; }

bool hasFlag(BuffData data, uint flag) { return (getFlags(data) & flag) > 0; }
void setFlag(inout BuffData data, uint flag) { setFlags(data, getFlags(data) | flag); }
void removeFlag(inout BuffData data, uint flag) { setFlags(data, getFlags(data) & ~flag); }

float getHeight(BuffData data) { return getIndex(data).y; }
float getLevel(BuffData data) { return getIndex(data).y * COLISIZE; }

void setInFrustum(BuffData data) { setFlag(data, 0x1); }
bool inFrustum(BuffData data) { return (getFlags(data) & 0x1) != 0; }

uint getType(BuffData data) { return (getFlags(data) >> 0x9) & 0xFF; }
uint getSides(BuffData data) { return (getFlags(data) >> 0x1) & 0xFF; }

//uint getLOD(campos, chunk_pos) { return uint(max(ceil(length(-iCamPos - chunk_pos)/10.0),0.0)); }
//float getLODScale(campos, chunk_pos) { return  max(float(getLOD(campos,chunk_pos))*0.1,1.0); }

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

bool is_chunk_visible(vec3 center)
{
  if (frustum) {
    int result = checkSphere(translate(center), 150);
    if (result < 0) { return false; }
  }
  return true;
}

bool is_visible(vec3 pos)
{
  //if(!is_chunk_visible()) return false;

  if (frustum) {
    int result = checkSphere(pos, 1.5);
    if (result < 0) return false;
  }
  return true;
}

#define MAXSIZE 2097152 //?^3

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