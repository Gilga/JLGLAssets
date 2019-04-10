module SCRIPT

using GLFW, ModernGL, SharedArrays

using ..GraphicsManager
using ..DefaultModelData
using ..CameraManager
using ..FrustumManager
using ..ChunkManager
using ..MeshManager
using ..TextureManager
using ..ShaderManager
using ..ScriptManager

using ..TimeManager
using ..LogManager
using ..Math

const GPU = GraphicsManager

WINDOW = nothing
prevTime = Ref(0.0)
FRAMES = 0
MAX_FRAMES = 0
ITERATION = 0

function showFrames()
  currenttime = time() #GetTimer("FRAME_TIMER")
  
  global FRAMES, MAX_FRAMES, ITERATION
  
  ITERATION +=1
  if !OnTime(1.0, prevTime; time=currenttime) FRAMES += 1; return end
  
  if MAX_FRAMES < FRAMES MAX_FRAMES = FRAMES end
  fps = FRAMES
  max_fps = MAX_FRAMES
  fpms = FRAMES > 0 ? (1000.0 / FRAMES) : 0
  max_fmps = MAX_FRAMES > 0 ? (1000.0 / MAX_FRAMES) : 0
  norm_fps = FRAMES / MAX_FRAMES
  
  GLFW.SetWindowTitle(WINDOW, "Test - FPS $(round(fps; digits=2))[$(round(max_fps; digits=2))] | FMPS $(round(fpms; digits=2))[$(round(max_fmps; digits=2))] - IT $ITERATION")
  FRAMES = 0
end

function rezizeWindow(window, width, height)
  GLFW.SetWindowSize(window, width, height)
  glViewport(0, 0, width, height)
end

WINDOW_FOCUS = true
function OnFocus(window, focus)
  global WINDOW_FOCUS = focus > 0 ? true : false
end

function OnUpdatedKeys()
  keyValue, keyPressed = getKey()
  if keyPressed
    resetKeys()
    
    if keyValue == 82 #r
      ScriptManager.reload()
      return false
    end
  end
  true
end

BUFFER = nothing
function init(inputs::Dict{Symbol,Any})
  global WINDOW = inputs[:WINDOW]
  rezizeWindow(WINDOW, 800, 600)
  global BUFFER = zeros(Float32,128^3*6*40) #createBuffer(zeros(Float32,6*128^3),40)
end

function clean()
  println("I got cleaned")
  global BUFFER = nothing
end

CLOSE = true
function update()
  UpdateTimers()
  if CLOSE return end
end
  
function render()
  showFrames()
  if !WINDOW_FOCUS
    sleep(0.1)
  else
    if !OnUpdatedKeys() return end
  end
end



end

