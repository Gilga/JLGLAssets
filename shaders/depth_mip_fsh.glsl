#import "globals.glsl"

// Want highp since depth is 24-bit.
precision highp float;

layout(location = 0) in vec2 vTex;
layout(location = 0) out vec4 FragColor;

layout(binding = 1) uniform sampler2D iDepthTexture;

void main() {
  vec4 depths = textureGather(iDepthTexture, vTex, 0);
  gl_FragDepth = max(max(depths.x, depths.y), max(depths.z, depths.w));
}