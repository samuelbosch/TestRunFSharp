namespace SB.TestRun

open System

module Program =   
    
    [<EntryPoint>]
    [<STAThread>]
    let Main(args) = 
        
        //MultiSwarmOptimization.run()
        //MultiSwarm.MultiSwarmProgram.Main(args)
        //RadialNetIO.run()
        
        //NistRandomness.NistProgram.Main(args)
        //Nist.run()
        
        LogisticRegressionNewtonRaphson.LogisticRegressionNRProgram.Main(args)
        LogisticRegressionNR.run()
        
        Console.WriteLine("Hit key to close")
        Console.ReadLine() |> ignore
        0