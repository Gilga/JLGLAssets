#import "globals.glsl"
#import "landscape.glsl"

#define DIST (1.0/(VOXEL_DIST*0.5))

precision highp sampler2DShadow;
precision highp sampler2D;

layout(location = 0) flat in uint index;
layout(location = 2) in float wave;
layout(location = 3) in vec3 pos;
layout(location = 4) in vec3 normal;

layout(location = 0) out vec4 outColor;

layout(binding = 0) uniform sampler2D iDepthMap;
layout(binding = 2) uniform sampler2D iTexturePack;
uniform int iDepth = 0;

layout(std430) buffer inputBuffer { BuffData instances[]; };

struct iMaterial {
  vec4 emission;    // Ecm, 16
  vec4 ambient;     // Acm, 16   
  vec4 diffuse;     // Dcm, 16   
  vec4 specular;    // Scm, 16   
  float shininess;  // Srm, 4 
} material;

struct iLight {
  vec4 color;		// 16
  vec3 position;	// 16
  float energy;	// 4
  int diffuse;	// 4
  int specular;	// 4
} light;

// void main() {
  // outColor = vec4(1);
// }

void main() {
  //int glow = v.texindex==15;
  
  BuffData data = instances[index];

  vec3 vertex_pos = pos;
  vec3 vertex_normal = normal; //normalize(vertex_pos);
  
  vec3 world_obj_pos = getPos(data) + iCenter + iPosition;
  vec3 world_pos = world_obj_pos + vertex_pos;
  
  vec4 color = getVertexColor(vertex_normal, 0); //color.w = color.x*color.y*color.z;
  
  vec3 cam_pos = -iCamPos;

  bool UseLight = true; //iUseLight;
  uint lightType = 1;
  bool UseTexture = false; //iUseTexture;
  bool water = false;
  
  //float dist = abs(length(getUV(vertex_pos)-vec2(0.5)));
  //if(dist>0.5) discard;

  if(true)
  {
    float level = getLevel(data);//+clamp(1+world_obj_pos.y/128.0,0,1);
    uint typ = getType(data);
    water = typ == 16;
  
    //float level_air = 0.99;
    float level_grass = 0.8;
    float level_dirt = 0.65;
    float level_stone = 0.5;
    float level_stonebricks = 0.4;
    float level_sand =  0.3;
    float level_lava =  0.2;
    float level_water =  0.42;
      
    //if (y >= level_air) typ = 0; // air or nothing
    if (level <= level_lava) { typ = 15; color = vec4(0.75,0,0,1); } //lava
    else if (level <= level_sand) { typ = 9; color = vec4(0.75,0.75,0,1); } //sand
    else if (level <= level_stonebricks) { typ = 5; color = vec4(0.5,0.5,0.6,1); } //stonebricks 
    else if (level <= level_stone) { typ = 4; color = vec4(0.7,0.7,0.8,1); } //stone
    else if (level <= level_dirt) { typ = 1; color = vec4(0.5,0.4,0.3,1); } //dirt
    else if (level <= level_grass) { typ = 2; color = vec4(0.2,0.4,0.1,1); } //grass
    else if (level > level_grass) { typ = 13; color = vec4(vec3(0.75),1); } //snow
 
    //if(isWater && level <= level_water) typ = 16;
    //typ *= (0.5+sin(world_obj_pos.x*(1.0/18.0)+iTime*0)*0.5);
    
    //water = typ == 16;
    if(water){ color = vec4(0.25,0.5,0.75,1); }
    
    if(UseTexture && !water){
      vec2 texUV = getTexUV(typ-1);
      vec2 UV = getUV(vertex_pos)*0.25f;
      
      // flip texture 
      UV.y=(1-UV.y);
      UV.x+=0.25f*texUV.x;
      UV.y+=-0.75f+0.25*texUV.y;
      UV = clamp(UV,0,1); // valid values otherwise might be break
      //UV = vec2(0,0);
      color = texture(iTexturePack, vec2(UV.y, UV.x));
    }
  }
  
  //if(!UseTexture && !water) color = vec4(vec3(0.5 + sin(world_pos.x*(1.0/18.0))*0.5, 0.5 + sin(world_pos.z*(1.0/18))*0.5,(0.5 + sin(world_pos.y*(1.0/28.0)))),1);
  float time = iTime*0+1;
  vec3 lightPos = vec3(sin(time)*300,200+sin(time*3)*100,cos(time)*300);
  vec3 lightDistVec = lightPos - world_pos;
  vec3 lightDir = normalize(lightDistVec);
  
  float lightDist = length(lightDistVec);
  
  float H2 = clamp(dot(lightDir, vertex_normal),0.0,1.0);
  float range = 1.0/lightDist;
  float alpha = radians(0);
  
  light.color = vec4(1,1,1,1);
  light.energy = 100;
  light.position = lightPos; //vec3(1000,500,-300); //vec3(sin(alpha),0,cos(alpha));
  light.diffuse = 1;
  light.specular = 1;
  
  float lightAttenuation = 0.01f;
  float gammaAmount = 2.2f;
  float shininessCoefficient = 1;

  material.emission = vec4(0.0,0.0,0.0,1.0); //vec4(glow?1:0,glow?0.5:0,0,1);
  material.ambient = vec4(0.0,0.0,0.0,1.0);
  material.diffuse = vec4(1.0,1.0,1.0,1.0);
  material.specular = vec4(1.0,1.0,1.0,1.0);
  material.shininess = 0;

  
  if(UseLight && !water && lightType == 1){ //use phong?
    vec3 L = lightDir; // Direction of the light (from the fragment to the light)
    vec3 N = vertex_normal; // Normal of the computed fragment, in camera space
 
    float H = clamp(dot(L, N),0.0,1.0);
    float diffuseCoefficient = H;
    
    vec3 ambient = vec3(0,0,0);
    vec3 diffuse = vec3(0,0,0);
    vec3 specular = vec3(0,0,0);
    vec3 emission = vec3(0,0,0);
    vec3 difSpec = vec3(0,0,0);
    
    //attenuation
    float distanceToLight = range;
    //if(distanceToLight <= 0.01) distanceToLight=0;
    float attenuation = distanceToLight * light.energy; //clamp(,0.0,1.0); // / (1.0 + lightAttenuation * pow(distanceToLight,2));
    
    // phong_weight
    if(diffuseCoefficient > 0.0)
    {
      if(light.diffuse > 0)
      {
        diffuse = material.diffuse.xyz * light.color.xyz * diffuseCoefficient * material.diffuse.w;
      }
      
      if(light.specular > 0 && shininessCoefficient > 0 && material.shininess > 0)
      {
        vec3 E = normalize(cam_pos - world_pos); // camera direction (towards the camera)
        vec3 R = reflect(-L, N); // Direction in which the triangle reflects the light
        float cosTheta = clamp(dot(E, R), 0.0, 1.0); //E, R, R, L
      
        float specularCoefficient = material.shininess > 0 ? pow( cosTheta, material.shininess * shininessCoefficient) : 0.0f;
        specular = material.specular.xyz * light.color.xyz * diffuseCoefficient * specularCoefficient * material.specular.w;
      }

      difSpec += (diffuse + specular);
    }

    emission = material.emission.xyz; //* emission Intensity
    ambient = material.ambient.xyz; //* ambient Intensity
    
    // replace alpha
    //ambient = vec4(mix(bgcolor.xyz,ambient.xyz, ambient.w*0.0),1);
    
    vec3 all = (emission  + ambient + mix(color.xyz*0.25,color.xyz,difSpec)) * attenuation * gammaAmount;
    color = vec4(clamp(all,0,1),color.w);

    //color = vec4(pow(all, vec3(gammaAmount)),color.w)
  }
  else if(UseLight && !water && lightType == 2) {
    float diff = clamp(dot(vertex_normal, lightDir), 0.0, 1.0);
    vec3 diffuse = light.color.xyz * diff * range * 100;
    color = vec4(diffuse,color.w);
  }
  else if(true) {
    if(water) {
      vec3 dir = normalize(cam_pos - world_pos);
      float r = clamp(range*100,0,1);
      if(r<0.01) r = 0; else if(r>0.99) r = 1;
      float a = (1-clamp(r*0.25,0,1)); //*clamp(1-1/distance(cam_pos,world_pos)*10,0,1); //UseLight ? range*100 : 0.75;
      //if(a<0.01) a = 0; else if(a>0.99) a = 1;
      color.rgb = color.xyz*mix(SEA_WATER_COLOR, SEA_BASE, (wave)) * r;//(1-texUV.y)*f
    
      /*
      vec3 light = normalize(vec3(0.0,1.0,0.8)); 
      vec3 E = normalize(cam_pos - world_pos);
      vec3 ldir = normalize(light - world_pos);
      
      // color
      color.rgb = mix(
      getSkyColor(ldir),
      getSeaColor(vertex_pos,vertex_normal,light,E,light - world_pos),
      pow(smoothstep(0.0,-0.05,ldir.y),0.3));*/
      
      
      color.a=a; //clamp(a*(0.5+sin(1-texUV.y*0.5)*0.5),0,1);
    }
    else color = vec4(vec3(color.xyz*range*100),color.w);
  }

  bool fog = false;
  
  if(fog) {
    float dist = distance(cam_pos,world_pos);
    float fogFactor = (dist/($CHUNK_SIZE*1.75)); //smoothstep(0.0f, 9.0f, dist/25);
    float old_alpha = color.a;
    color = mix(color,vec4(0.25,0.25,0.25,0.0),clamp(pow(fogFactor,10),0,1));
   //if(water)  color.a = old_alpha * color.a;
  }
  
  if(iDepth == 1){
    float depth = gl_FragCoord.z;
    //depth = 1 - (1.0/(length(cam_pos - world_pos)));
    vec2 duv = gl_FragCoord.xy / textureSize(iDepthMap,0);
    //float tdepth = texture(iDepthTexture, duv).x;
    float tdepth = texture(iDepthMap, duv).x + 0.00000003;
    //float tdepth = clamp(textureLod(iDepthMap, vec3(duv, 0), 1) + 0.00000003 ,0,1); //0.00000003 not sure why its needed
    if(tdepth<depth) discard;
    //else { depth = gl_FragCoord.z; }
    //gl_FragDepth = depth;
  }

  outColor = color;
}