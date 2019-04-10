#import "globals.glsl"

#define SEED 43758.5453123
#define NUM_OCTAVES 5

float random (in float n) { return fract(sin(n)*43758.5453123); }
float random (in vec2 _st) { return fract(sin(dot(_st.xy,vec2(12.9898,78.233)))*43758.5453123); }

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

float fbm ( in vec2 _st) {
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


layout(location = 0) out vec4 outColor;

void main() {
  vec2 uv = gl_FragCoord.xy / iResolution.xy + vec2(sin(iTime*0.01*random(1)),sin(iTime*0.01*random(2)));
  float cloud = clamp(fbm(uv*3.0)*1.0,0,1);
  outColor = vec4(vec3(cloud),1);
}