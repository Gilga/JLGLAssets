#version 460
layout(binding = 0) uniform sampler2D iDepthTexture;
uniform bool iDepth = false;

void main() {

  //if (iDepth) {
  //  float tdepth = clamp(texture(iDepthTexture, gl_FragCoord.xy / textureSize(iDepthTexture,0)).x + 0.00000003 ,0,1);
  //  if(tdepth<gl_FragCoord.z) discard;
  //  gl_FragDepth = gl_FragCoord.z;
  //}

  gl_FragDepth = gl_FragCoord.z;
}