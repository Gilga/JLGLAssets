#import "globals.glsl"

layout(triangles) in;
layout(line_strip, max_vertices=126) out; // 128 is hardware max

layout(location = 0) in vec4 vcolors[];
out vec4 vcolor;
//layout(location = 0) in Vertex vertex[];

void main()
{
  int max = 3;
  int count = max*max*max;
  float dist = 2;
  float center = 0; //(max/2)*dist;
  
  float x=0,y=0,z=0,c=0;
  for(int c=0; c<count; ++c)
  {
    c=0;
    for(int i=0; i<gl_in.length(); ++i)
    {
      vcolor = vcolors[i];
      if(vcolor.w > 0) {
        gl_Position = iMVP * (gl_in[i].gl_Position + vec4(-center+x*dist,-center+y*dist,-center+z*dist,0));
        EmitVertex();
        c++;
      } // else discard
    }
    
    if(c>0) EndPrimitive(); // else discard
    
    ++x;
    if(x>=max) {
      x=0; ++y;
      if(y>=max) {y=0; ++z;}
    }
  }
}