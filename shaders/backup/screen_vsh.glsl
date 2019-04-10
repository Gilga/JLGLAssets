#version 460
layout (location = 0) in vec2 iVertex;
layout (location = 0) out vec2 texCoord;

void main() {
  texCoord = iVertex * 0.5f + 0.5f;
  gl_Position = vec4(iVertex.x, iVertex.y, -0.001, 1.0);
}