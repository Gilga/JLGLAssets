precision highp int;
precision highp float;

#define COLSIZE $CHUNK1D_SIZE //?^1
#define ROWSIZE $CHUNK2D_SIZE //?^2
#define MAXSIZE $CHUNK3D_SIZE //?^3
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