#import "buffer.glsl"

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