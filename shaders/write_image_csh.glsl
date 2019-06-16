layout( local_size_x = 16, local_size_y = 16, local_size_z = 1) in;
layout(binding = 0, rgba32f) uniform writeonly image2D img_output;

uniform float iTime = 1;

void main() {
  // base pixel colour for image
  vec4 pixel = vec4(0.0, 0.0, 1.0, 1.0);
  // get index in global work group i.e x,y position
  vec2 norm_coordinates = (gl_GlobalInvocationID.xy + vec2(0.5)) / vec2(imageSize(img_output));
  ivec2 pixel_coords = ivec2(gl_GlobalInvocationID.xy);
  float localCoef = length(vec2(ivec2(gl_LocalInvocationID.xy)-16)/16.0);
  float globalCoef = sin(float(gl_WorkGroupID.x+gl_WorkGroupID.y)*0.1 + iTime)*0.5;
  float g = gl_GlobalInvocationID.x * gl_GlobalInvocationID.y;
  float c = sin(g * 1 + iTime*3); //1.0-(gl_GlobalInvocationID.x*(gl_GlobalInvocationID.y+1)); //globalCoef*localCoef;
  pixel = vec4(0.0, c, c, 1.0);
  //pixel = vec4(norm_coordinates.x, norm_coordinates.y, 0.0, 1.0);

  // output to a specific pixel in the image
  imageStore(img_output, pixel_coords, pixel);
}
