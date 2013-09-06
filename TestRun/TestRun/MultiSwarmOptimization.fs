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
    let inline (..*) (a:float list) b = List.map2 ((*)) a b

    let updateParticle multiSwarm bestMultiPos bestSwarmPos (particle:Particle) =   
        let w  = 0.729
        let c1, c2, c3 = 1.49445,1.49445, 0.3645
        let dim = particle.Position.Length
        let createR() = List.init multiSwarm.Dim (fun i -> rand.NextDouble())
        let r1, r2, r3 = createR(), createR(), createR()
        let newVelocity = (w .* particle.Velocity)
                            ++ (c1 .* r1 ..* (particle.Best.Pos -- particle.Position))
                            ++ (c2 .* r2 ..* (bestSwarmPos -- particle.Position))
                            ++ (c3 .* r3 ..* (bestMultiPos -- particle.Position))
        // keep velocity within bounds
        let newVelocity = newVelocity |> List.map ((max multiSwarm.MinX) >> (min multiSwarm.MaxX))
        // update position
        let newPosition = particle.Position ++ newVelocity
        let newCost = cost newPosition
        
        if newCost < particle.Best.Cost then
            let newBest = {Pos=newPosition; Cost=newCost }
            Some(newBest), { particle with Velocity = newVelocity; Position=newPosition; Best=newBest}
        else
            None, { particle with Velocity = newVelocity; Position=newPosition }
    
    let deathOrAlive multiSwarm particle = 
        let death = 0.005 // prob of particle death
        if rand.NextDouble() < death then
            let newParticle = Particle.create multiSwarm.Dim multiSwarm.MinX multiSwarm.MaxX
            newParticle
        else
            particle

    let updateSwarm multiSwarm bestMultiPos swarm =
        let folder (particle:Particle) state = //state makes sure that we always know what the best particle cost of this swarm is
            let best, particles = state

            let death = 0.005 // prob of particle death
            let particle = deathOrAlive multiSwarm particle    
            
            let newBest, newParticle = updateParticle multiSwarm bestMultiPos best.Pos particle
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
        // TODO implement immigration
        let folder (swarm:Swarm) state = //state makes sure that we always know what the best swarm cost is
            let best, swarms = state

            let newBest, newSwarm = updateSwarm multiSwarm best.Pos swarm
            if newBest.IsSome then
                newBest.Value, (newSwarm::swarms)
            else
                best, (newSwarm::swarms)
        let newBest, swarms = List.foldBack folder multiSwarm.Swarms (multiSwarm.Best,List.empty)
        if newBest.Cost < multiSwarm.Best.Cost then
            { multiSwarm with Swarms = swarms; Best=newBest}
        else
            { multiSwarm with Swarms = swarms }

    let immigration (swarms:Swarm list) (i, j) = 
        let particleA = swarms.[i].Particles.[j]
        let otheri = rand.Next(0, swarms.Length)
        let otherj = rand.Next(0, swarms.Head.Particles.Length)
        let particleB = swarms.[otheri].Particles.[otherj]
        
        let substituteParticle newParticle index swarm = { swarm with Particles = ( List.mapi (fun i particle -> if i = index then newParticle else particle) swarm.Particles) }
        let swapParticle currentIndex swarm  = 
            if currentIndex = i then 
                (substituteParticle particleB j swarm) 
            else if currentIndex = otheri then
                (substituteParticle particleA otherj swarm) 
            else swarm
        List.mapi swapParticle swarms


    let rec solve (multiSwarm:MultiSwarm) maxloop ij = 
        if maxloop <= 0 then
            multiSwarm
        else
            let updatedMultiSwarm = updateMultiSwarm multiSwarm
            solve updatedMultiSwarm (maxloop-1) ij
        
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
        let i_range = [0 .. numSwarms-1]
        let j_range = [0 .. numParticles-1]
        let ij = i_range |> List.collect (fun i -> j_range |> List.map (fun j -> (i,j)))
        let ms = solve ms maxLoop ij
        printfn "Final multiswarm:"
        printfn "%A" ms
        printfn "Best solution found with cost = %f at x0 = %f, x1 = %f" ms.Best.Cost ms.Best.Pos.[0] ms.Best.Pos.[1]
        
        printfn "End demo multi swarm optimization"