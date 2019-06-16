#import "mc.glsl"

layout(location = 0) out vec4 fragColor;

void main() {
  fragColor =  texture(iChannel0, gl_FragCoord.xy / iResolution.xy);
}
