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

layout (local_size_x = 128) in; //, local_size_y = 4, local_size_z = 4

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