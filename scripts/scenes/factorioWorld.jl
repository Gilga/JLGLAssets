__precompile__(false)

module SCRIPT

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
  this.vars[:WIDTH] = width
  this.vars[:HEIGHT] = height
  this.vars[:RATIO] = width / (height*1f0)
  this.vars[:SIZE] = width * height
  this.vars[:RESOLUTION] = Float32[width,height]

  GLFW.SetWindowSize(window, width, height)
  glViewport(0, 0, width, height)
end

""" TODO """
function OnInit()
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
  glClearColor(0.0, 1.0, 0.0, 0.0)

  this.vars[:SCREEN_MESH] = MeshData()
  linkData(this.vars[:SCREEN_MESH], :vertices=>(DATA_PLANE2D_VERTEX_STRIP,2))

  reloadShaderPrograms()
end

""" TODO """
function OnReload()
end

""" TODO """
function OnUpdate()
  checkForUpdate()
end

""" TODO """
function OnRender()
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT)

  useShaderProgram(:SCREEN)
  glBindVertexArray(this.vars[:SCREEN_MESH].vao)
  setShaderProperty("iTime", Float32(programTime()))
  setShaderProperty("iResolution", this.vars[:RESOLUTION])
  glDrawArrays(GL_TRIANGLE_STRIP, 0, this.vars[:SCREEN_MESH].draw.count)
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
  sh_global_vars = typeof(this.vars)()
  #sh_global_vars[:CHUNK_SIZE] = 0
  this.vars[:SH_GLOBAL_VARS] = sh_global_vars

  println("Load shader files...")
  shaders = loadShaders(sh_global_vars)

  SCREEN_VSH = shaders[:SCREEN_VSH]
  FACTORIO_BG_FSH = shaders[:FACTORIO_BG_FSH]
  #FACTORIO_BG2_FSH = shaders[:FACTORIO_BG2_FSH]

  ShaderManager.reloadProgram(this.shaderManager, :SCREEN, [SCREEN_VSH, FACTORIO_BG_FSH])
  setAttributes(this.vars[:SCREEN_MESH], getShaderProgram(:SCREEN))
end

end # SCRIPT
