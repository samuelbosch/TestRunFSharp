namespace SB.TestRun

open System

// based on http://msdn.microsoft.com/en-us/magazine/dn385711.aspx (September 2013 Issue)
module MultiSwarmOptimization = 

    let cost = 
        let rastrigin x = ((x*x) - (10.0 * (cos (2.0 * Math.PI * x))) + 10.0)
        List.sumBy rastrigin
    
    let rand = new Random(1)

    type Best = { 
        Pos: float list; 
        Cost: float 
    }

    type Particle =  { 
        Position: float list; 
        Velocity: float list;
        Best: Best;
    }
    with
        static member create dim minX maxX =
            let initializer i = (maxX - minX) * rand.NextDouble() + minX
            let position = List.init dim initializer
            let velocity = List.init dim initializer
            { Position = position; Velocity = velocity; Best = { Cost=(cost position); Pos = (List.map id position)  }}
    end

    type Swarm = {
        Particles: Particle list;
        Best: Best;
    }
    with 
        static member create numParticles dim minX maxX = 
            let particles = List.init numParticles (fun i -> Particle.create dim minX maxX)
            let bestPart = particles |> List.minBy (fun p -> p.Best.Cost) |> (fun p -> p.Best)
            { Particles = particles; Best=bestPart }
    end

    type MultiSwarm = {
        Swarms: Swarm list;
        Best: Best;
        Dim: int;
        MinX: float;
        MaxX: float;
    }
    with 
        static member create numSwarms numParticles dim minX maxX =
            let swarms = List.init numSwarms (fun i -> Swarm.create numParticles dim minX maxX)
            let bestSwarm = swarms |> List.minBy (fun s -> s.Best.Cost) |> (fun s -> s.Best)
            { Swarms = swarms; Dim=dim; MinX=minX;MaxX=maxX; Best=bestSwarm }
    end

    let inline (.*) (a:float) b = List.map ((*) a) b
    let inline (++) (a:float list) b = List.map2 (+) a b
    let inline (--) (a:float list) b = List.map2 (-) a b
    
    let updateParticle bestMultiPos bestSwarmPos (particle:Particle) = 
        let w  = 0.729
        let c1, c2, c3 = 1.49445,1.49445, 0.3645
        let r1, r2, r3 = rand.NextDouble(), rand.NextDouble(), rand.NextDouble()
        let newVelocity = (w .* particle.Velocity)
                            ++ (c1 * r1 .* (particle.Best.Pos -- particle.Position))
                            ++ (c2 * r2 .* (bestSwarmPos -- particle.Position))
                            ++ (c3 * r3 .* (bestMultiPos -- particle.Position))
        let newPosition = particle.Position ++ newVelocity
        let newCost = cost newPosition
        
        if newCost < particle.Best.Cost then
            let newBest = {Pos=newPosition; Cost=newCost }
            Some(newBest), { particle with Velocity = newVelocity; Position=newPosition; Best=newBest}
        else
            None, { particle with Velocity = newVelocity; Position=newPosition }

    
    let updateSwarm bestMultiPos swarm =
        let folder (particle:Particle) state = //state makes sure that we always now the up to date best particle cost of this swarm
            let best, particles = state
            let newBest, newParticle = updateParticle bestMultiPos best.Pos particle
            if newBest.IsSome then
                newBest.Value, (newParticle::particles)
            else
                best, (newParticle::particles)
        
        let newBest, particles = List.foldBack folder swarm.Particles (swarm.Best,List.empty)
        if newBest.Cost < swarm.Best.Cost then
            Some(newBest), { swarm with Particles = particles; Best=newBest}
        else
            None, { swarm with Particles = particles }

    let updateMultiSwarm multiSwarm =
        // TODO implement death and immigration
        let folder (swarm:Swarm) state = //state makes sure that we always now the up to date best swarm cost
            let best, swarms = state
            let newBest, newSwarm = updateSwarm best.Pos swarm
            if newBest.IsSome then
                newBest.Value, (newSwarm::swarms)
            else
                best, (newSwarm::swarms)
        let newBest, swarms = List.foldBack folder multiSwarm.Swarms (multiSwarm.Best,List.empty)
        if newBest.Cost < multiSwarm.Best.Cost then
            { multiSwarm with Swarms = swarms; Best=newBest}
        else
            { multiSwarm with Swarms = swarms }

    
    let rec solve (multiSwarm:MultiSwarm) maxloop = 
        if maxloop <= 0 then
            multiSwarm
        else
            let newMultiSwarm = updateMultiSwarm multiSwarm
            solve newMultiSwarm (maxloop-1)
        
    let run() =
        printfn "Begin Multiple Particle Swarm optimization demo"
        let dim = 2
        let minX = -100.0
        let maxX = 100.0
        let numParticles = 4 // Particles in each swarm
        let numSwarms = 3 // Swarms in multi-swarm
        let ms = MultiSwarm.create numSwarms numParticles dim minX maxX
        printfn "Initial multiswarm:"
        printfn "%A" ms
        let maxLoop = 150
        let ms = solve ms maxLoop
        printfn "Final multiswarm:"
        printfn "%A" ms
        printfn "Best solution found with cost = %f at x0 = %f, x1 = %f" ms.Best.Cost ms.Best.Pos.[0] ms.Best.Pos.[1]
        
        printfn "End demo multi swarm optimization"