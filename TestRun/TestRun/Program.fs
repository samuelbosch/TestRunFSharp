namespace SB.TestRun

open System

module Program =   
    
    [<EntryPoint>]
    [<STAThread>]
    let Main(args) = 
        
        MultiSwarmOptimization.run()
        MultiSwarm.MultiSwarmProgram.Main(args)
        Console.WriteLine("Hit key to close")
        Console.ReadLine() |> ignore
        0