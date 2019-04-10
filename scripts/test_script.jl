module SCRIPT

BUFFER = zeros(Float32,128^3*400)

function init(inputs::Dict{Symbol,Any})
	FILE = Base.@__FILE__
  println("Script: $(basename(FILE))")
  global BUFFER = zeros(Float32,128^3*6*40)
end

function update()
end

function render()
end

end #SCRIPT
