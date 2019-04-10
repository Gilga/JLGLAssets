module Game

using CodeManager

function run()
  println("Create module TEST...")
  #eval(:(module TEST; x=zeros(Float32,128^3*300); end))

  CodeManager.include_module(@__MODULE__, joinpath(@__DIR__,"script.jl"))
  println("Done.")
end

function reload()
  println("Reload module Script in 5 Seconds...")
  sleep(5)
  CodeManager.safe_clean!(@__MODULE__, :Script)
  CodeManager.include_module(@__MODULE__, joinpath(@__DIR__,"script.jl"))
  println("Done.")
end

function cleanUp()
  println("Clean module Script in 5 Seconds...")
  sleep(5)
  CodeManager.safe_clean!(@__MODULE__, :Script)
  println("Done.")
end

end
