__precompile__(false)

module SCRIPT

using DataStructures #OrderedDict

using GLFW, ModernGL, SharedArrays, StaticArrays, Quaternions

using RessourceManager
#using WindowManager
using GraphicsManager
using DefaultModelData
using CameraManager
using FrustumManager
using ChunkManager
using MeshManager
using TextureManager
using ShaderManager
#using ..ScriptManager
using TimeManager
using LogManager
using MathManager

const GPU = GraphicsManager

mutable struct Script
  args::Dict{Symbol,Any}
  vars::Dict{Symbol,Any}
  shaderManager::JShaderManager
  Script() = new(Dict(),Dict(),JShaderManager())
end

this = Script()

function reset()
  this.vars = typeof(this.vars)()
  delPrograms(this.shaderManager)
end

""" TODO """
function main(args::Dict{Symbol,Any})
  this.args = args
  println("Script: $(basename(@__FILE__)), $(this.args), time: $(mtime(@__FILE__))")
  reset()
end

""" TODO """
function OnDestroy()
  println("OnDestroy")
  reset()
  GraphicsManager.cleanUp() #remove all
  GC.gc()
end

function resizeWindow(window, width, height)
  this.vars[:RESOLUTION] = (
    width = width,
    height = height,
    ratio = width / (height*1f0),
    size = width * height
  )

  GLFW.SetWindowSize(window, width, height)
  glViewport(0, 0, width, height)
end

""" TODO """
function OnInit()
  SS = this.vars[:SECTION_SHADER] = Dict{Symbol,Any}()
  SM = this.vars[:SECTION_MESH] = Dict{Symbol,Any}()
  ST = this.vars[:SECTION_TEXTURES] = Dict{Symbol,Any}()

  UF = SS[:UNIFORMS] = OrderedDict{Symbol,Any}()
  #UF = SS[:UNIFORMS] = OrderedDict{Symbol,Any}()
  #TS = ST[:TEXTURES] = OrderedDict{Symbol,Any}()

  resizeWindow(this.args[:WINDOW], 800, 600)

  glEnable(GL_DEPTH_TEST)
  glEnable(GL_BLEND)
  glEnable(GL_CULL_FACE)
  #glEnable(GL_MULTISAMPLE)

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  #glBlendEquation(GL_FUNC_ADD)
  #glFrontFace(GL_CCW)

  glCullFace(GL_BACK) #default: GL_BACK
  glDepthMask(GL_TRUE) #default: GL_TRUE
  glDepthFunc(GL_LEQUAL) #default: GL_LESS

  #glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)#GL_FILL,GL_LINE
  glClearColor(0.5, 0.5, 0.5, 0.0)

  resolution = values(this.vars[:RESOLUTION])[1:2]

  UF[:iResolution] = Uniform(value=Float32[resolution...,1])
  UF[:iTime] = Uniform(value=Float32(0),default=0)
  UF[:iFrame] = Uniform(value=Float32(0),default=0)
  UF[:iTimeDelta] = Uniform(value=Float32(0),default=0)
  UF[:iFrameRate] = Uniform(value=Float32(0),default=0)
  UF[:iChannelTime] = Uniform(value=zeros(Float32,4))
  UF[:iMouse] = Uniform(value=zeros(Float32,4))
  UF[:iDate] = Uniform(value=zeros(Float32,4))
  UF[:iSampleRate] = Uniform(value=Float32(0))
  UF[:iChannelResolution] = Uniform(value=[zeros(Float32,3),zeros(Float32,3),zeros(Float32,3),zeros(Float32,3)])
  UF[:iChannel0] = Uniform(value=Int32(0),typ=GL_SAMPLER_2D,layout=Dict(:binding=>0))
  UF[:iChannel1] = Uniform(value=Int32(1),typ=GL_SAMPLER_2D,layout=Dict(:binding=>1))
  UF[:iChannel2] = Uniform(value=Int32(2),typ=GL_SAMPLER_2D,layout=Dict(:binding=>2))
  UF[:iChannel3] = Uniform(value=Int32(3),typ=GL_SAMPLER_2D,layout=Dict(:binding=>3))

  this.vars[:renderTime] = this.vars[:startTime] = time()

  SM[:SCREEN] = MeshData()
  linkData(SM[:SCREEN], :vertices=>(DATA_PLANE2D_VERTEX_STRIP,2))

  size = (UInt32(resolution[1]), UInt32(resolution[2]))

  txp(path::String) = RessourceManager.getPath(:TEXTURES, path)

  global iChannel0=createTexture(:iChannel0, size) #txp("A.png"))
  global iChannel1=createTexture(:iChannel1, size) #txp("B.png"))
  global iChannel2=createTexture(:iChannel2, size) #txp("C.png"))
  global iChannel3=createTexture(:iChannel3, size) #txp("D.png"))

  glBindImageTexture(0, iChannel0, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32F)
  glBindImageTexture(1, iChannel1, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32F)
  glBindImageTexture(2, iChannel2, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32F)
  glBindImageTexture(3, iChannel3, 0, GL_FALSE, 0, GL_WRITE_ONLY, GL_RGBA32F)

  SS[:PROGRAMS] = OrderedDict(
  :SCREEN => (shaders=[:SCREEN_VSH, :MC_FSH],attributes=[:SCREEN],),
  :MC_BA => (shaders=[:MC_BA_CSH],),
  :MC_BB => (shaders=[:MC_BB_CSH],),
  :MC_BC => (shaders=[:MC_BC_CSH],),
  :MC_BD => (shaders=[:MC_BD_CSH],),
  )

  dispatch = (x=round(UInt32, resolution[1]/16f0), y=round(UInt32, resolution[2]/16f0), z=UInt32(1))
  dispatchInFile = (x=16,y=16,z=1)

  SS[:DISPATCH] = [
  (:MC_BA, dispatch),
  (:MC_BB, dispatch),
  (:MC_BC, dispatch),
  (:MC_BD, dispatch),
  ]

  SS[:GLOBALS] = Dict(
  :iDispatchX=>dispatchInFile.x,
  :iDispatchY=>dispatchInFile.y,
  :iDispatchZ=>dispatchInFile.z,
  )

  ST[:TEXTURES] = OrderedDict(
  :iChannel0 => (location=0,),
  :iChannel1 => (location=1,),
  :iChannel2 => (location=2,),
  :iChannel3 => (location=3,),
  )

  reloadShaderPrograms()
end

""" TODO """
function OnReload()
end

""" TODO """
function OnUpdate()
  checkForUpdate()

  UF = this.vars[:SECTION_SHADER][:UNIFORMS]

  deltaTime = Float32(this.vars[:renderTime] - this.vars[:startTime]) #now timeIntervalSinceDate:time()
  if deltaTime > 1f0/20f0
      deltaTime = 1f0/20f0
  end

  UF[:iTimeDelta].value = Float32(deltaTime)
  UF[:iFrameRate].value = Float32(1f0/deltaTime)
end

function setShaderProperties()
  for (k,v) in this.vars[:SECTION_SHADER][:UNIFORMS]
    setShaderProperty(string(k), v.value)
  end
end

function bindTextures()
  for (k,v) in this.vars[:SECTION_TEXTURES][:TEXTURES]
    bindTexture(k, v.location)
  end
end

function dispatchPrograms(list)
  for (p,d) in list
    useShaderProgram(p)
    setShaderProperties()
    glDispatchCompute(d.x, d.y, d.z)
    glMemoryBarrier(GL_SHADER_IMAGE_ACCESS_BARRIER_BIT)
  end
end

""" TODO """
function OnRender()
  SS = this.vars[:SECTION_SHADER]
  SM = this.vars[:SECTION_MESH]
  UF = SS[:UNIFORMS]

  iframe = UF[:iFrame]
  UF[:iTime].value = Float32(programTime())

  bindTextures()

  dispatchPrograms(SS[:DISPATCH])

  mesh = SM[:SCREEN]

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)
  useShaderProgram(:SCREEN)
  glBindVertexArray(mesh.vao)
  setShaderProperties()
  glDrawArrays(GL_TRIANGLE_STRIP, 0, mesh.draw.count)

  UF[:iFrame].value += 1
  this.vars[:renderTime] = time()
end

""" TODO """
function checkForUpdate()
  keyValue, keyPressed = getKey()

  if keyPressed
    if keyValue == 45 #ß
    elseif keyValue == 48 #0
    elseif keyValue >= 49 && keyValue <= 57 #1-9

    elseif keyValue == 65 #a
    elseif keyValue == 66 #b
    elseif keyValue == 67 #c
    elseif keyValue == 68 #d
    elseif keyValue == 69 #e
    elseif keyValue == 70 #f
    elseif keyValue == 71 #g
    elseif keyValue == 72 #h
    elseif keyValue == 73 #i
    elseif keyValue == 74 #j
    elseif keyValue == 75 #k
    elseif keyValue == 76 #l
    elseif keyValue == 77 #m
    elseif keyValue == 78 #n
    elseif keyValue == 79 #o
    elseif keyValue == 80 #p
    elseif keyValue == 81 #q
    elseif keyValue == 82 #r
      reloadShaderPrograms()
    elseif keyValue == 83 #s
    elseif keyValue == 84 #t
    elseif keyValue == 85 #u
    elseif keyValue == 86 #v
    elseif keyValue == 86 #w
    elseif keyValue == 87 #x
    elseif keyValue == 88 #y
    elseif keyValue == 89 #z

    elseif (keyValue >= 290 && keyValue <= 301) # F1 - F12
    end
  end
end

useShaderProgram(program::Union{Nothing, Symbol}) = ShaderManager.useProgram(this.shaderManager, program)
getShaderProgram(program::Symbol) = ShaderManager.getProgram(this.shaderManager, program).id
setShaderProperty(name::String, value::Any; program::Union{Nothing,Symbol}=getActiveProgram(this.shaderManager)) = ShaderManager.setProperty(this.shaderManager, name::String, value::Any; program=program)
#if mode != "" println("MODE(",stringColor(mode;color=:yellow),"): ",stringColor(value;color=:yellow)) end

function reloadShaderPrograms()
  println("Load shader files...")
  SS = this.vars[:SECTION_SHADER]
  SM = this.vars[:SECTION_MESH]

  shaders = loadShaders(SS)

  for (p,t) in SS[:PROGRAMS]
    ShaderManager.reloadProgram(this.shaderManager, p, (x->shaders[x]).(t.shaders))
    programID = getShaderProgram(p)
    if haskey(t,:attributes)
      for a in t.attributes
        setAttributes(SM[a], programID)
      end
    end
  end

end

end # SCRIPT
