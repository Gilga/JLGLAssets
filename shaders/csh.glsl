#import "globals.glsl"
#import "landscape.glsl"

#define texSize 1f/1024f

layout (local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) uniform writeonly image2D oHeightMap;

void main() {
  ivec2 pixel_coords = ivec2(gl_GlobalInvocationID.xy);
  float localCoef = length(vec2(ivec2(gl_LocalInvocationID.xy)-8)/8.0);
  float globalCoef = sin(float(gl_WorkGroupID.x+gl_WorkGroupID.y)*0.1 + iTime)*0.5;
  float height = createLandscapeHeight(vec2(pixel_coords * texSize )); //1.0-globalCoef*localCoef
  vec4 pixel = vec4(vec3(height), 0.0);
  imageStore(oHeightMap, pixel_coords, pixel);
}