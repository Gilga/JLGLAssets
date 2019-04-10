#import "globals.glsl"

layout(location = 0) out vec4 outColor;

//layout(binding = 0) uniform sampler2DShadow iDepthMap;
layout(binding = 1) uniform sampler2D iDepthTexture;
layout(binding = 2) uniform sampler2D iTexturePack;
layout(binding = 3) uniform sampler2D iHeightTexture;

vec2 invert(vec2 uv, bool invertX, bool invertY) {
  return vec2(invertX ? 1-uv.x : uv.x, invertY ? 1-uv.y : uv.y);
}

vec2 rotate(vec2 uv) {
  return vec2(1-uv.y,uv.x);
}

vec2 rotate_back(vec2 uv) {
  return vec2(uv.y,1-uv.x);
}

vec2 getCoord(vec2 uv, vec2 scale, vec2 move, bool r, bool invertX) {
  uv = invert(uv, invertX, false);
  uv = rotate_back(rotate(uv) / vec2(1,iResolution.y/iResolution.x) / scale - move.yx);
  if (r) { uv=rotate(uv); uv=invert(uv, invertX, invertX); }
  uv = invert(uv, invertX, false);
  return uv;
}

bool isValid(vec2 uv) {
  return !(uv.x < 0 || uv.y < 0 || uv.x > 1 || uv.y > 1);
}

void main() {
 vec2 uv = (gl_FragCoord.xy / iResolution.xy);
 
 float visible=0;
 vec3 color = vec3(0);
 
 vec2 current = vec2(0);
 vec2 scale = vec2(0.25);
 
 current = getCoord(uv, scale, vec2(0.03+2,0.01), true, true);
 if(isValid(current)) {
  color = texture(iTexturePack, current).xyz;
  visible=1;
 }
 
 current = getCoord(uv, scale, vec2(0.01+0,0.01), false, true);
 if(isValid(current)) {
  color = (1 - texture(iDepthTexture, (gl_FragCoord.xy/ textureSize(iDepthTexture,0))).xyz)*50000; //0.22
  visible=1;
 }
 
 current = getCoord(uv, scale, vec2(0.02+1,0.01), false, true);
 if(isValid(current)) {
  color = (1 - texture(iHeightTexture, current).xyz);
  visible=1;
 }
 
 /*
 current = getCoord(uv, scale, vec2(0.04+3,0.01), false, true);
 if(isValid(current)) {
  color = (1 - texture(iHeightMap, current).xyz);
  visible=1;
 }*/
 
 outColor = vec4(color,visible);
}