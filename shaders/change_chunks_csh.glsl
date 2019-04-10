#import "globals.glsl"
#import "landscape.glsl"
#import "dispatch.glsl"

precision highp float;
precision highp int;
precision highp sampler2DShadow;
precision highp sampler2D;

uniform int STATE = 0;
layout(binding = 0) uniform sampler2DShadow iDepthMap;

uniform int iCulling = 0;
uniform bool iRasterrize = false;

layout (local_size_x = $CHUNK_SIZE) in;

layout(binding = 0, offset = 0) uniform atomic_uint DISPATCH;
layout(binding = 1, offset = 0) uniform atomic_uint dispatchCount;
layout(binding = 2, offset = 0) uniform atomic_uint instanceCount;
layout(binding = 3, offset = 0) uniform atomic_uint counter;

layout(std430, binding = 0) buffer inputBuffer { BuffData inputData[]; };
layout(std430, binding = 1) buffer outputBuffer { BuffData outputData[]; };

void setDispatch()
{
  //synchronize();
  uint count = atomicCounter(dispatchCount);
  //+0.000130653854284902*0
  uint dispatch = uint(round((float(count)/MAXSIZE)*DISPATCHSIZE));
  //if(dispatch<=0) dispatch=1; // DO NOT REMOVE THIS LINE OR DISPATCH RESET WILL FAIL! (FIXED)
  atomicCounterExchange(DISPATCH, dispatch); // dispatch now
}

BuffData data;
MapData mapflags;
bool visible;
uint unique;
uint dispatch;
uint ident;
vec3 pos;
uint flags;

uint CHUNK_COUNT = 25; // 9, 25, 49, 81, 121

float calculateLOD(vec3 chunk_pos){
  float len = length((-iCamPos - translate(chunk_pos+vec3(COLSIZE*0.5)))*vec3(1,0,1));
  return 1; //max(ceil(len/256.0),1.0); // uint
}
 
void main() {
  ident  = gl_GlobalInvocationID.x;
  
  if(STATE < 0) // RESET
  {
    if(STATE == -1) { if(ident == 0) atomicCounterExchange(DISPATCH, DISPATCHSIZE); return;  }
    else if(STATE == -2) { /*if(ident == 0) setDispatch();*/ return;  }
    else if(STATE == -3) { /*if(ident == 0) setDispatch();*/ return;  }
    else if(STATE == -4) { /*if(ident == 0) atomicCounterExchange(instanceCount, 0);*/ return; }
    return;
  }
  else if(STATE < 3) // SET
  {
    if(STATE == 0) // INIT
    { 
      if(ident == 0) {
        atomicCounterExchange(dispatchCount, 0);
        atomicCounterExchange(instanceCount, 0);
        atomicCounterExchange(counter, 0);
      }
      
      vec3 index;
      vec3 pos;
      vec3 chunk_pos;
      float lod;
      float scale;
      float cut;
      //vec3 tex_index = getIndex2DPos(ident); 
      vec3 tex_index = getIndexPos(ident); //tex_index.y=0;
      vec3 base_index = tex_index + iCenter;
      
      //if(ident>=ROWSIZE) return;
    
      for(uint chunk=0; chunk<CHUNK_COUNT; chunk++)
      {
        chunk_pos = CHUNK_POSITIONS[chunk]*COLSIZE;
        
        lod = calculateLOD(chunk_pos);
        scale = 1.0 / lod;
        cut = scale * COLSIZE;
        
        index = chunk_pos*scale + base_index;
        
        //outputData[ident] = createBuffData();

        if(base_index.x>cut || base_index.z>cut || base_index.y>cut) continue;
        
        mapflags = getValidBlock(index, lod);
        float height = 0; //getLandscapeHeight(index.xz,1); mapflags.height=1; mapflags.sides=127;
        
        if (mapflags.height >= 0 && mapflags.sides > 0) {
          flags = convertFlags(mapflags);
          ident += uint(height * ROWSIZE);
          
          inputData[atomicCounterIncrement(dispatchCount)] = createBuffData(ident,chunk,flags);
          
          if (mapflags.sides > 0) {
            pos = translate(index); // //*(vec3(1,0,1)*1.0/scale+vec3(0,1,0));
            if(is_visible(pos))
            outputData[atomicCounterIncrement(instanceCount)] = createBuffData(ident,chunk,flags);
          }
        }
      }
      
      // set new dispatch
      //if(ident == LAST) setDispatch();
      return;
    }
    else if(STATE == 1) // change frustum
    {
      if(ident == 0) {
        atomicCounterExchange(instanceCount, 0);
        atomicCounterExchange(counter, 0);
      }
      
      for(uint chunk=0; chunk<CHUNK_COUNT; chunk++){
        unique = atomicCounterIncrement(counter);
        if(unique<=atomicCounter(dispatchCount))
        {
          data = inputData[unique];
          if (inFrustum(data) || is_visible(getPos(data))) {
            outputData[atomicCounterIncrement(instanceCount)] = data;
            setInFrustum(data);
            inputData[unique] = data;
          }
        }
      }
    }
    else if(STATE == 13) // UPDATE FRUSTUM
    {
      if(ident == 0) atomicCounterExchange(instanceCount, 0);
      if(ident>atomicCounter(dispatchCount)) return;
      
      data = inputData[ident];
      
      vec3 index = getPos(data);
      vec3 pos = translate(index);
      bool visible = is_visible(pos);

      /*
      vec3 campos = vec3(iCamPos.x,0,iCamPos.z);
      vec3 len=-campos-pos;
      
      float range = COLSIZE;
      
      bool valid_x = abs(len.x) <= range;
      bool valid_z = abs(len.z) <= range;
      bool valid_y = abs(len.y) <= range;
      
      if (flags.height >= 0)
      
      bool invalid = true;
      
      if (valid_x && valid_z && valid_y) {
        if(data.height < 0) {
          
          //index.y=flags.height;
        }
        else invalid = false;
      }
      else {
        if(!valid_x) index.x = (len.x > 0 ? 1 : -1 ) * range + index.x;
        if(!valid_z) index.z = (len.z > 0 ? 1 : -1 ) * -range + index.z;
        //if(!valid || !valid_y) index.y = (len.y > 0 ? 1 : -1 ) * range + index.y;
        flags = getTypeSide2(index);
      }
      
      if(invalid) inputData[ident] = createBuffData(pos,0,flags.sides,-1);
      if(invalid) return;
      */
      
      //if (!visible) return;
      
      unique = atomicCounterIncrement(instanceCount);
      //setPos(data, pos);
      //data.flags = unique;
      removeFlag(data,VISIBLE_FLAG);
      outputData[unique] = data;
      
      return;
    }
    else if(STATE == 23) // UPDATE OCCLUSION CULLING
    {
      if(ident == 0) {
        atomicCounterExchange(instanceCount, 0);
        atomicCounterExchange(counter, 0);
      }
      
      unique = atomicCounterIncrement(counter);
      if(unique>atomicCounter(dispatchCount)) return;

      data = outputData[unique];
      synchronize();
      
      if(iRasterrize && (!hasFlag(data,VISIBLE_FLAG))) {
        //outputData[ident].sides=0;
        return;
      }

      unique = atomicCounterIncrement(instanceCount);
      outputData[unique] = data;
      
      //flags = MapData(data.type,data.sides,data.height);
      //index = getIndexPos(ident) + iCenter * COLSIZE * 0;
      //flags = getTypeSide2(index);
      //pos = translate(index);
      //visible = is_visible(pos);
      
      //if (data.height < 0 || !visible)  return;
      //unique  = atomicCounterIncrement(instanceCount);
      //outputData[unique] = createData(pos,unique,data.sides,data.height);
      return;
    }
    return;
    
    /*
    float d = COLSIZE*0.78;
    vec3 campos = vec3(iCamPos.x,0,iCamPos.z); //+vec3(d,COLSIZE*0.5*0,d)*vec3(sin(iCamAng.x),sin(iCamAng.y)*0,cos(iCamAng.x));

    vec3 len=-campos-pos;
    
    float range = COLSIZE;
    
    bool valid_x = abs(len.x) <= range;
    bool valid_z = abs(len.z) <= range;
    bool valid_y = abs(len.y) <= range;

    bool change = true;
      
    //if (valid && distance(-campos,pos) <= range) {
    if (valid_x && valid_z && valid_y) {
      if(flags.type == -2) {
        flags = getTypeSide2(index);
        //index.y=flags.height;
      }
      else change = false;
    }
    else {
      if(!valid_x) index.x = (len.x > 0 ? 1 : -1 ) * range + index.x;
      if(!valid_z) index.z = (len.z > 0 ? 1 : -1 ) * -range + index.z;
      //if(!valid || !valid_y) index.y = (len.y > 0 ? 1 : -1 ) * range + index.y;
      flags.type = -2;
    }
    
    if(change) {
      if(STATE == 0) dinputData[dispatch] = Data(float[3](index.x,index.y,index.z),flags.type,flags.sides,-1);
      else inputData[ident] = Data(float[3](index.x,index.y,index.z),flags.type,flags.sides,-1);
    }
    if(change) return;
    */
    /*
    if (flags.height < 0 || !visible) return;
    
    if(iCulling == 2) {
      pos = getPos(data);
      vec2 zNearFar = vec2(gl_DepthRange.near,gl_DepthRange.far);

      float radius = 0.5;
      vec3 view_center = (iView * vec4(pos, 1.0)).xyz;
      float nearest_z = view_center.z + radius;
      
      // Sphere clips against near plane, just assume visibility.
      if (nearest_z >= -zNearFar.x) return;
      
      vec3 view_center_norm = normalize(view_center);
      
      float az_plane_horiz_length = length(view_center.xz);
      float az_plane_vert_length = length(view_center.yz);
      vec2 az_plane_horiz_norm = view_center.xz / az_plane_horiz_length;
      vec2 az_plane_vert_norm = view_center.yz / az_plane_vert_length;
      
      vec2 t = sqrt(vec2(az_plane_horiz_length, az_plane_vert_length) * vec2(az_plane_horiz_length, az_plane_vert_length) - radius * radius);
      vec4 w = vec4(t, radius, radius) / vec2(az_plane_horiz_length, az_plane_vert_length).xyxy;
      
      // Fairly optimized way to apply the two rotation matrices.
      // Since the two rotation matrices are almost the same (just flipped sign of sin()), we can reuse some computation.
      vec4 horiz_cos_sin = az_plane_horiz_norm.xyyx * t.x * vec4(w.xx, -w.z, w.z);
      vec4 vert_cos_sin  = az_plane_vert_norm.xyyx * t.y * vec4(w.yy, -w.w, w.w);

      vec2 horiz0 = horiz_cos_sin.xy + horiz_cos_sin.zw;
      vec2 horiz1 = horiz_cos_sin.xy - horiz_cos_sin.zw;
      vec2 vert0  = vert_cos_sin.xy + vert_cos_sin.zw;
      vec2 vert1  = vert_cos_sin.xy - vert_cos_sin.zw;

      // This assumes the projection matrix doesn't do translations or any other transforms first.
      vec4 projected = -0.5 * vec4(iProj[0][0], iProj[0][0], iProj[1][1], iProj[1][1]) *
          vec4(horiz0.x, horiz1.x, vert0.x, vert1.x) /
          vec4(horiz0.y, horiz1.y, vert0.y, vert1.y) + 0.5;

      // Since we know which way we're rotating to find the tangent points, we already know which one is min and max.
      vec2 min_xy = projected.yw;
      vec2 max_xy = projected.xz;
      
      // Project our nearest Z value in view space.
      vec2 zw = mat2(iProj[2].zw, iProj[3].zw) * vec2(nearest_z, 1.0);
      
      float dist = (1/(length(-iCamPos-pos)));
      
      nearest_z = 1 + (-1 + zw.x / zw.y) * 0.52; //clamp(dist,0.5,1.0);

      //vec4 clip_space_pos = iProj * vec4(view_center+vec3(0,0,radius),1.0);
      //nearest_z = 1 + (-1 + clip_space_pos.z / clip_space_pos.w) * 0.53;
      //nearest_z = (((zNearFar.y-zNearFar.x) * nearest_z) + zNearFar.x + zNearFar.y) / 2.0;
       
      ivec2 lodSize = textureSize(iDepthMap, 0);
       
      // Compute required LOD factor for shadow lookup.
      vec2 diff_pix = (max_xy - min_xy) * lodSize;
      float max_diff = max(max(diff_pix.x, diff_pix.y), 1.0);
      float lod = ceil(log2(max_diff));

      vec2 mid_pix = 0.5 * (max_xy + min_xy);
      float tdepth = textureLod(iDepthMap, vec3(mid_pix, nearest_z), lod);
      //tdepth = texelFetch(iDepthMap,clamp(ivec2(mid_pix), ivec2(0), lodSize - ivec2(1)), int(lod)).r;
      tdepth=clamp(tdepth,0,1);
      //float tdepth = clamp(texture(iDepthTexture,mid_pix).x,0,1);
      
      //if(nearest_z>0.99995) return;
      
      //if(mid_pix.x <= 0.45 || mid_pix.x >= 0.55) return;
      //if(tdepth<=nearest_z) return; // Test visibility.
      if(tdepth<=0.0) return; // Test visibility.
    }
    
    unique  = atomicCounterIncrement(instanceCount);
    //if(unique==0) { flags.type=9999; flags.sides=255; } // SUN
    
    outputData[unique] = create(data,flags);
    */
  
  } else {
  /*
    //if(ident == 0) atomicCounterExchange(counter, 0);
    //if(ident == 0) atomicCounterExchange(instanceCount, 0);
    //if(atomicCounter(counter) >= atomicCounter(instanceCount)) return;
    
    data = inputData[ident];
    flags = MapData(data.type,data.sides,data.height);
 
    //unique = atomicCounterIncrement(instanceCount);
    outputData[ident] = create(data,flags);
    */
  }
}