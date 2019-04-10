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

#define SEED 43758.5453123
#define NUM_OCTAVES 5

layout(binding = 3) uniform sampler2D iHeightTexture;

struct MapData {
  float type;
  float sides;
  float height;
};

//BuffData create(BuffData data, MapData flags) { return BuffData(data.pos,flags.type,flags.sides,flags.height); }

uint convertFlags(MapData flags) { return (uint(flags.type) << 0x9) | (uint(flags.sides) << 0x1); }

float random (in float n) { return fract(sin(n)*43758.5453123); }
float random (in vec2 _st) { return fract(sin(dot(_st.xy,vec2(12.9898,78.233)))*43758.5453123); }

float hash( vec2 p ) {
	float h = dot(p,vec2(127.1,311.7));	
    return fract(sin(h)*43758.5453123);
}
float wave_noise( in vec2 p ) {
    vec2 i = floor( p );
    vec2 f = fract( p );	
	vec2 u = f*f*(3.0-2.0*f);
    return -1.0+2.0*mix( mix( hash( i + vec2(0.0,0.0) ), 
                     hash( i + vec2(1.0,0.0) ), u.x),
                mix( hash( i + vec2(0.0,1.0) ), 
                     hash( i + vec2(1.0,1.0) ), u.x), u.y);
}

// sea
float sea_octave(vec2 uv, float choppy) {
    uv += wave_noise(uv);        
    vec2 wv = 1.0-abs(sin(uv));
    vec2 swv = abs(cos(uv));    
    wv = mix(wv,swv,wv);
    return pow(1.0-pow(wv.x * wv.y,0.65),choppy);
}

// sea
const int ITER_GEOMETRY = 3;
const int ITER_FRAGMENT = 5;
const float SEA_HEIGHT = 0.6;
const float SEA_CHOPPY = 4.0;
const float SEA_SPEED = 0.8;
const float SEA_FREQ = 0.16;
const vec3 SEA_BASE = vec3(0.1,0.19,0.22);
const vec3 SEA_WATER_COLOR = vec3(0.8,0.9,0.6);
#define SEA_TIME (1.0 + iTime * SEA_SPEED)
const mat2 octave_m = mat2(1.6,1.2,-1.2,1.6);

const int NUM_STEPS = 8;
//const float EPSILON	= 1e-3;

float map(vec3 p) {
    float freq = SEA_FREQ;
    float amp = SEA_HEIGHT;
    float choppy = SEA_CHOPPY;
    vec2 uv = p.xz; uv.x *= 0.75;
    
    float d, h = 0.0;    
    for(int i = 0; i < ITER_GEOMETRY; i++) {        
    	d = sea_octave((uv+SEA_TIME)*freq,choppy);
    	d += sea_octave((uv-SEA_TIME)*freq,choppy);
        h += d * amp;        
    	uv *= octave_m; freq *= 1.9; amp *= 0.22;
        choppy = mix(choppy,1.0,0.2);
    }
    return p.y - h;
}

float heightMapTracing(vec3 ori, vec3 dir, out vec3 p) {  
    float tm = 0.0;
    float tx = 1000.0;    
    float hx = map(ori + dir * tx);
    if(hx > 0.0) return tx;   
    float hm = map(ori + dir * tm);    
    float tmid = 0.0;
    for(int i = 0; i < NUM_STEPS; i++) {
        tmid = mix(tm,tx, hm/(hm-hx));                   
        p = ori + dir * tmid;                   
    	float hmid = map(p);
		if(hmid < 0.0) {
        	tx = tmid;
            hx = hmid;
        } else {
            tm = tmid;
            hm = hmid;
        }
    }
    return tmid;
}

// lighting
float ldiffuse(vec3 n,vec3 l,float p) {
    return pow(dot(n,l) * 0.4 + 0.6,p);
}
float lspecular(vec3 n,vec3 l,vec3 e,float s) {    
    float nrm = (s + 8.0) / (PI * 8.0);
    return pow(max(dot(reflect(e,n),l),0.0),s) * nrm;
}

// sky
vec3 getSkyColor(vec3 e) {
    e.y = max(e.y,0.0);
    return vec3(pow(1.0-e.y,2.0), 1.0-e.y, 0.6+(1.0-e.y)*0.4);
}

vec3 getSeaColor(vec3 p, vec3 n, vec3 l, vec3 eye, vec3 dist) {  
    float fresnel = clamp(1.0 - dot(n,-eye), 0.0, 1.0);
    fresnel = pow(fresnel,3.0) * 0.65;
        
    vec3 reflected = getSkyColor(reflect(eye,n));    
    vec3 refracted = SEA_BASE + ldiffuse(n,l,80.0) * SEA_WATER_COLOR * 0.12; 
    
    vec3 color = mix(refracted,reflected,fresnel);
    
    float atten = max(1.0 - dot(dist,dist) * 0.001, 0.0);
    color += SEA_WATER_COLOR * (p.y - SEA_HEIGHT) * 0.18 * atten;
    
    color += vec3(lspecular(n,l,eye,60.0));
    
    return color;
}

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

vec2 texSize = vec2(1.0/1024.0);

float createLandscapeHeight(vec2 uv) {
  return clamp(fbm(uv*2),0.0,1.0);
}

float getLandscapeHeight(vec2 uv) {
  return clamp(texture(iHeightTexture, uv).r,0.0,1.0);
}

float getLandscapeHeight(vec2 uv, float scale) {
  return clamp(fbm(uv * scale)* 1/scale,0.0,1.0);
  //return clamp(texture(iHeightTexture, (uv * scale)).r * 1/scale,0.0,1.0);
}


  /*
  float typ = -1;
  
  //float level_air = height * 0.99;
  float level_grass = height * 0.95;
  float level_dirt = height * 0.9;
  float level_stonebricks = height * 0.6;
  float level_stone = height * 0.5;
  float level_lava = height * fbm(index.xz*0.002)*1.1;

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
  */


MapData getValidBlock(vec3 index, float scale){
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
  
  float height = getLandscapeHeight(uv,scale);
  float h = (y==0?1:y)/height;
  int typ=0;
  uint sides=0;
  uint count=0;
  float lh = 0;
  float rh = 0;
  float fh = 0;
  float bh = 0;

  if(left) { lh=getLandscapeHeight(vec2(next.x,uv.y),scale); left = y <= lh; lh=height-lh; if(lh<0) lh=0; }
  if(right) { rh=getLandscapeHeight(vec2(next.y,uv.y),scale); right = y <= rh; rh=height-rh; if(rh<0) rh=0; }
  if(forward) { fh=getLandscapeHeight(vec2(uv.x,next.z),scale); forward = y <= fh; fh=height-fh; if(fh<0) fh=0; }
  if(back) { bh=getLandscapeHeight(vec2(uv.x,next.w),scale); back = y <= bh; bh=height-bh; if(bh<0) bh=0; }
  if(top) { top = nextTB.x <= height; }
  if(bottom) { bottom = nextTB.y <= height; }
  
  if(left && right && forward && back && top && bottom) return MapData(-1,0,-1);
  
  if(!left) { sides |= (0x1 << 0); count++; } // LEFT
  if(!right) {sides |= (0x1 << 1); count++; } // RIGHT
  if(!top) { sides |= (0x1 << 2);  count++; }// TOP
  if(!bottom) { sides |= (0x1 << 3);  count++; }// BOTTOM
  if(!forward) { sides |= (0x1 << 4);  count++; }// FRONT
  if(!back) { sides |= (0x1 << 5);  count++; } // BACK

  //if(!bottom && y>=0.41 && y<=0.42) { h=1; typ=16; sides=4; }
  
  if (sides <= 0 || h>1) h=-1;
  //else if ((y+S<height)) { h=-1; sides=0; }
  //else h = 1+max(max(lh,rh), max(fh,bh));
  else h=index.y; //*scale
  
  if (height <= 0) { sides = 0; }
  
  return MapData(typ,sides,h);
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
  
  if(left && right && forward && back && top && bottom) return vec3(-1,0,-1);
  
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
  
  return vec3(typ,sides,height*COLSIZE);
}

#define DIST (1.0/(VOXEL_DIST*0.5))

precision highp sampler2DShadow;
precision highp sampler2D;

layout(location = 0) flat in uint index;
layout(location = 2) in float wave;
layout(location = 3) in vec3 pos;
layout(location = 4) in vec3 normal;

layout(location = 0) out vec4 outColor;

layout(binding = 0) uniform sampler2D iDepthMap;
layout(binding = 2) uniform sampler2D iTexturePack;
uniform int iDepth = 0;

layout(std430) buffer inputBuffer { BuffData instances[]; };

struct iMaterial {
  vec4 emission;    // Ecm, 16
  vec4 ambient;     // Acm, 16   
  vec4 diffuse;     // Dcm, 16   
  vec4 specular;    // Scm, 16   
  float shininess;  // Srm, 4 
} material;

struct iLight {
  vec4 color;		// 16
  vec3 position;	// 16
  float energy;	// 4
  int diffuse;	// 4
  int specular;	// 4
} light;

// void main() {
  // outColor = vec4(1);
// }

void main() {
  //int glow = v.texindex==15;
  
  BuffData data = instances[index];

  vec3 vertex_pos = pos;
  vec3 vertex_normal = normal; //normalize(vertex_pos);
  
  vec3 world_obj_pos = getPos(data) + iCenter + iPosition;
  vec3 world_pos = world_obj_pos + vertex_pos;
  
  vec4 color = getVertexColor(vertex_normal, 0); //color.w = color.x*color.y*color.z;
  
  vec3 cam_pos = -iCamPos;

  bool UseLight = true; //iUseLight;
  uint lightType = 1;
  bool UseTexture = false; //iUseTexture;
  bool water = false;
  
  //float dist = abs(length(getUV(vertex_pos)-vec2(0.5)));
  //if(dist>0.5) discard;

  if(true)
  {
    float level = getLevel(data);//+clamp(1+world_obj_pos.y/128.0,0,1);
    uint typ = getType(data);
    water = typ == 16;
  
    //float level_air = 0.99;
    float level_grass = 0.8;
    float level_dirt = 0.65;
    float level_stone = 0.5;
    float level_stonebricks = 0.4;
    float level_sand =  0.3;
    float level_lava =  0.2;
    float level_water =  0.42;
      
    //if (y >= level_air) typ = 0; // air or nothing
    if (level <= level_lava) { typ = 15; color = vec4(0.75,0,0,1); } //lava
    else if (level <= level_sand) { typ = 9; color = vec4(0.75,0.75,0,1); } //sand
    else if (level <= level_stonebricks) { typ = 5; color = vec4(0.5,0.5,0.6,1); } //stonebricks 
    else if (level <= level_stone) { typ = 4; color = vec4(0.7,0.7,0.8,1); } //stone
    else if (level <= level_dirt) { typ = 1; color = vec4(0.5,0.4,0.3,1); } //dirt
    else if (level <= level_grass) { typ = 2; color = vec4(0.2,0.4,0.1,1); } //grass
    else if (level > level_grass) { typ = 13; color = vec4(vec3(0.75),1); } //snow
 
    //if(isWater && level <= level_water) typ = 16;
    //typ *= (0.5+sin(world_obj_pos.x*(1.0/18.0)+iTime*0)*0.5);
    
    //water = typ == 16;
    if(water){ color = vec4(0.25,0.5,0.75,1); }
    
    if(UseTexture && !water){
      vec2 texUV = getTexUV(typ-1);
      vec2 UV = getUV(vertex_pos)*0.25f;
      
      // flip texture 
      UV.y=(1-UV.y);
      UV.x+=0.25f*texUV.x;
      UV.y+=-0.75f+0.25*texUV.y;
      UV = clamp(UV,0,1); // valid values otherwise might be break
      //UV = vec2(0,0);
      color = texture(iTexturePack, vec2(UV.y, UV.x));
    }
  }
  
  //if(!UseTexture && !water) color = vec4(vec3(0.5 + sin(world_pos.x*(1.0/18.0))*0.5, 0.5 + sin(world_pos.z*(1.0/18))*0.5,(0.5 + sin(world_pos.y*(1.0/28.0)))),1);
  float time = iTime*0+1;
  vec3 lightPos = vec3(sin(time)*300,200+sin(time*3)*100,cos(time)*300);
  vec3 lightDistVec = lightPos - world_pos;
  vec3 lightDir = normalize(lightDistVec);
  
  float lightDist = length(lightDistVec);
  
  float H2 = clamp(dot(lightDir, vertex_normal),0.0,1.0);
  float range = 1.0/lightDist;
  float alpha = radians(0);
  
  light.color = vec4(1,1,1,1);
  light.energy = 100;
  light.position = lightPos; //vec3(1000,500,-300); //vec3(sin(alpha),0,cos(alpha));
  light.diffuse = 1;
  light.specular = 1;
  
  float lightAttenuation = 0.01f;
  float gammaAmount = 2.2f;
  float shininessCoefficient = 1;

  material.emission = vec4(0.0,0.0,0.0,1.0); //vec4(glow?1:0,glow?0.5:0,0,1);
  material.ambient = vec4(0.0,0.0,0.0,1.0);
  material.diffuse = vec4(1.0,1.0,1.0,1.0);
  material.specular = vec4(1.0,1.0,1.0,1.0);
  material.shininess = 0;

  
  if(UseLight && !water && lightType == 1){ //use phong?
    vec3 L = lightDir; // Direction of the light (from the fragment to the light)
    vec3 N = vertex_normal; // Normal of the computed fragment, in camera space
 
    float H = clamp(dot(L, N),0.0,1.0);
    float diffuseCoefficient = H;
    
    vec3 ambient = vec3(0,0,0);
    vec3 diffuse = vec3(0,0,0);
    vec3 specular = vec3(0,0,0);
    vec3 emission = vec3(0,0,0);
    vec3 difSpec = vec3(0,0,0);
    
    //attenuation
    float distanceToLight = range;
    //if(distanceToLight <= 0.01) distanceToLight=0;
    float attenuation = distanceToLight * light.energy; //clamp(,0.0,1.0); // / (1.0 + lightAttenuation * pow(distanceToLight,2));
    
    // phong_weight
    if(diffuseCoefficient > 0.0)
    {
      if(light.diffuse > 0)
      {
        diffuse = material.diffuse.xyz * light.color.xyz * diffuseCoefficient * material.diffuse.w;
      }
      
      if(light.specular > 0 && shininessCoefficient > 0 && material.shininess > 0)
      {
        vec3 E = normalize(cam_pos - world_pos); // camera direction (towards the camera)
        vec3 R = reflect(-L, N); // Direction in which the triangle reflects the light
        float cosTheta = clamp(dot(E, R), 0.0, 1.0); //E, R, R, L
      
        float specularCoefficient = material.shininess > 0 ? pow( cosTheta, material.shininess * shininessCoefficient) : 0.0f;
        specular = material.specular.xyz * light.color.xyz * diffuseCoefficient * specularCoefficient * material.specular.w;
      }

      difSpec += (diffuse + specular);
    }

    emission = material.emission.xyz; //* emission Intensity
    ambient = material.ambient.xyz; //* ambient Intensity
    
    // replace alpha
    //ambient = vec4(mix(bgcolor.xyz,ambient.xyz, ambient.w*0.0),1);
    
    vec3 all = (emission  + ambient + mix(color.xyz*0.25,color.xyz,difSpec)) * attenuation * gammaAmount;
    color = vec4(clamp(all,0,1),color.w);

    //color = vec4(pow(all, vec3(gammaAmount)),color.w)
  }
  else if(UseLight && !water && lightType == 2) {
    float diff = clamp(dot(vertex_normal, lightDir), 0.0, 1.0);
    vec3 diffuse = light.color.xyz * diff * range * 100;
    color = vec4(diffuse,color.w);
  }
  else if(true) {
    if(water) {
      vec3 dir = normalize(cam_pos - world_pos);
      float r = clamp(range*100,0,1);
      if(r<0.01) r = 0; else if(r>0.99) r = 1;
      float a = (1-clamp(r*0.25,0,1)); //*clamp(1-1/distance(cam_pos,world_pos)*10,0,1); //UseLight ? range*100 : 0.75;
      //if(a<0.01) a = 0; else if(a>0.99) a = 1;
      color.rgb = color.xyz*mix(SEA_WATER_COLOR, SEA_BASE, (wave)) * r;//(1-texUV.y)*f
    
      /*
      vec3 light = normalize(vec3(0.0,1.0,0.8)); 
      vec3 E = normalize(cam_pos - world_pos);
      vec3 ldir = normalize(light - world_pos);
      
      // color
      color.rgb = mix(
      getSkyColor(ldir),
      getSeaColor(vertex_pos,vertex_normal,light,E,light - world_pos),
      pow(smoothstep(0.0,-0.05,ldir.y),0.3));*/
      
      
      color.a=a; //clamp(a*(0.5+sin(1-texUV.y*0.5)*0.5),0,1);
    }
    else color = vec4(vec3(color.xyz*range*100),color.w);
  }

  bool fog = false;
  
  if(fog) {
    float dist = distance(cam_pos,world_pos);
    float fogFactor = (dist/(128*1.75)); //smoothstep(0.0f, 9.0f, dist/25);
    float old_alpha = color.a;
    color = mix(color,vec4(0.25,0.25,0.25,0.0),clamp(pow(fogFactor,10),0,1));
   //if(water)  color.a = old_alpha * color.a;
  }
  
  if(iDepth == 1){
    float depth = gl_FragCoord.z;
    //depth = 1 - (1.0/(length(cam_pos - world_pos)));
    vec2 duv = gl_FragCoord.xy / textureSize(iDepthMap,0);
    //float tdepth = texture(iDepthTexture, duv).x;
    float tdepth = texture(iDepthMap, duv).x + 0.00000003;
    //float tdepth = clamp(textureLod(iDepthMap, vec3(duv, 0), 1) + 0.00000003 ,0,1); //0.00000003 not sure why its needed
    if(tdepth<depth) discard;
    //else { depth = gl_FragCoord.z; }
    //gl_FragDepth = depth;
  }

  outColor = color;
}