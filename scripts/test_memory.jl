module App
  include("../src/CodeManager.jl")
  using .CodeManager

  #delete_old() = Core.eval(@__MODULE__, :(module TEST end))
  
  function run()
    println("Create module Game...")
    loaded = false
    loaded = CodeManager.include_module(@__MODULE__, joinpath(@__DIR__,"game.jl"))[1]
    
    if loaded
      println("Invoke Game.run()...")
      CodeManager.safe_invoke(@__MODULE__, :(Game.run))
      println("Invoke Game.reload()...")
      CodeManager.safe_invoke(@__MODULE__, :(Game.reload))
      println("Invoke Game.cleanUp()...")
      CodeManager.safe_invoke(@__MODULE__, :(Game.cleanUp))
    end
    
    println("Clean module Game in 5 Seconds...")
    sleep(5)
    CodeManager.safe_clean!(@__MODULE__, :Game)
    println("Done.")
  end

end

App.run()
