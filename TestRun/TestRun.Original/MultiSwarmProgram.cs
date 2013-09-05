// demo of multi-swarm optimization

using System;
namespace MultiSwarm
{
    public class MultiSwarmProgram
    {
        public static Random ran = new Random(1);

        public static void Main(string[] args)
        {

            try
            {
                Console.WriteLine("\nBegin Multiple Particle Swarm optimization demo\n");
                Console.WriteLine("Goal is to minimize Rastrigin's function");
                Console.WriteLine("Function has known solution of 0.0 at x0 = 0.0, x1 = 0.0");

                Console.WriteLine("\nSetting minX = -100.0 and maxX = +100.0");
                int dim = 2;
                double minX = -100.0;
                double maxX = 100.0;

                Console.WriteLine("Setting number particles in each swarm = 4");
                Console.WriteLine("Setting number swarms in multiswarm = 3");
                int numParticles = 4; // number particles in each swarm
                int numSwarms = 3; // number swarms in multiswarm

                Console.WriteLine("\nInitializing all swarms in multiswarm");
                Multiswarm ms = new Multiswarm(numSwarms, numParticles, dim, minX, maxX);
                Console.WriteLine("\nInitial multiswarm:");
                Console.WriteLine(ms.ToString());

                int maxLoop = 150;
                Console.WriteLine("\nSetting maxLoop = " + maxLoop);
                Console.WriteLine("Entering main solve loop");
                ms.Solve(maxLoop);

                Console.WriteLine("Solve loop complete");
                Console.WriteLine("\nFinal multiswarm:");
                Console.WriteLine(ms.ToString());

                Console.WriteLine("\nBest solution found = " + ms.bestMultiCost.ToString("F6"));
                Console.Write("at x0 = " + ms.bestMultiPos[0].ToString("F4"));
                Console.WriteLine(", x1 = " + ms.bestMultiPos[1].ToString("F4"));

                Console.WriteLine("\nEnd demo\n");
                Console.ReadLine();
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
                Console.ReadLine();
            }
        }

        public static double Cost(double[] position)
        {
            double result = 0.0;
            for (int i = 0; i < position.Length; ++i)
            {
                double xi = position[i];
                result += (xi * xi) - (10 * Math.Cos(2 * Math.PI * xi)) + 10;
            }
            return result;
        }

    } // Program

    public class Particle
    {

        public double[] position;
        public double[] velocity;
        public double cost;
        public double[] bestPartPos;
        public double bestPartCost;

        public Particle(int dim, double minX, double maxX)
        {
            position = new double[dim];
            velocity = new double[dim];
            bestPartPos = new double[dim];
            for (int i = 0; i < dim; ++i)
            {
                position[i] = (maxX - minX) * MultiSwarmProgram.ran.NextDouble() + minX;
            }
            for (int i = 0; i < dim; ++i)
            {
                velocity[i] = (maxX - minX) * MultiSwarmProgram.ran.NextDouble() + minX;
            }
            cost = MultiSwarmProgram.Cost(position);
            bestPartCost = cost;
            Array.Copy(position, bestPartPos, dim);
        }

        public override string ToString()
        {
            string s = "";
            s += "Pos [ ";
            for (int i = 0; i < position.Length; ++i)
                s += position[i].ToString("F2") + " ";
            s += "] ";
            s += "Vel [ ";
            for (int i = 0; i < velocity.Length; ++i)
                s += velocity[i].ToString("F2") + " ";
            s += "] ";
            s += "Cost = " + cost.ToString("F3");
            s += " Best Pos [ ";
            for (int i = 0; i < bestPartPos.Length; ++i)
                s += bestPartPos[i].ToString("F2") + " ";
            s += "] ";
            s += "BestCost = " + cost.ToString("F3");
            return s;
        }

    } // Particle

    public class Swarm
    {
        public Particle[] particles;
        public double[] bestSwarmPos;
        public double bestSwarmCost;

        public Swarm(int numParticles, int dim, double minX, double maxX)
        {
            bestSwarmCost = double.MaxValue;
            bestSwarmPos = new double[dim];
            particles = new Particle[numParticles];
            for (int i = 0; i < particles.Length; ++i)
            {
                particles[i] = new Particle(dim, minX, maxX);
                if (particles[i].cost < bestSwarmCost)
                {
                    bestSwarmCost = particles[i].cost;
                    Array.Copy(particles[i].position, bestSwarmPos, dim);
                }
            }
        }

        public override string ToString()
        {
            string s = "";
            for (int i = 0; i < particles.Length; ++i)
                s += "[" + i + "] " + particles[i].ToString() + "\n";
            s += "Best Swarm Pos [ ";
            for (int i = 0; i < bestSwarmPos.Length; ++i)
                s += bestSwarmPos[i].ToString("F2") + " ";
            s += "] ";
            s += "Best Swarm Cost = " + bestSwarmCost.ToString("F3");
            s += "\n";
            return s;
        }
    } // Swarm

    public class Multiswarm
    {
        public Swarm[] swarms;
        public double[] bestMultiPos;
        public double bestMultiCost;
        public int dim;
        public double minX;
        public double maxX;

        public Multiswarm(int numSwarms, int numParticles, int dim, double minX, double maxX)
        {
            swarms = new Swarm[numSwarms];
            bestMultiPos = new double[dim];
            bestMultiCost = double.MaxValue;
            this.dim = dim;
            this.minX = minX;
            this.maxX = maxX;

            for (int i = 0; i < numSwarms; ++i)
            {
                swarms[i] = new Swarm(numParticles, dim, minX, maxX);
                if (swarms[i].bestSwarmCost < bestMultiCost)
                {
                    bestMultiCost = swarms[i].bestSwarmCost;
                    Array.Copy(swarms[i].bestSwarmPos, bestMultiPos, dim);
                }
            }
        }

        public void Solve(int maxLoop)
        {
            int ct = 0;
            double w = 0.729; // inertia
            double c1 = 1.49445; // particle / cogntive
            double c2 = 1.49445; // swarm / social
            double c3 = 0.3645; // multiswarm / global
            double death = 0.005; ; // prob of particle death
            double immigrate = 0.005;  // prob of particle immigration

            while (ct < maxLoop)
            {
                ++ct;
                for (int i = 0; i < swarms.Length; ++i) // each swarm
                {
                    for (int j = 0; j < swarms[i].particles.Length; ++j) // each particle
                    {
                        double p = MultiSwarmProgram.ran.NextDouble();
                        if (p < death)
                        {
                            swarms[i].particles[j] = new Particle(dim, minX, maxX);
                        }

                        double q = MultiSwarmProgram.ran.NextDouble();
                        if (q < immigrate)
                        {
                            Immigration(i, j); // swap curr particle with a random particle in diff swarm
                        }

                        for (int k = 0; k < dim; ++k) // update velocity. each x position component
                        {
                            double r1 = MultiSwarmProgram.ran.NextDouble();
                            double r2 = MultiSwarmProgram.ran.NextDouble();
                            double r3 = MultiSwarmProgram.ran.NextDouble();

                            swarms[i].particles[j].velocity[k] = (w * swarms[i].particles[j].velocity[k]) +
                              (c1 * r1 * (swarms[i].particles[j].bestPartPos[k] - swarms[i].particles[j].position[k])) +
                              (c2 * r2 * (swarms[i].bestSwarmPos[k] - swarms[i].particles[j].position[k])) +
                              (c3 * r3 * (bestMultiPos[k] - swarms[i].particles[j].position[k]));

                            if (swarms[i].particles[j].velocity[k] < minX)
                                swarms[i].particles[j].velocity[k] = minX;
                            else if (swarms[i].particles[j].velocity[k] > maxX)
                                swarms[i].particles[j].velocity[k] = maxX;

                        }

                        for (int k = 0; k < dim; ++k) // update position
                        {
                            swarms[i].particles[j].position[k] += swarms[i].particles[j].velocity[k];
                        }

                        // update cost
                        swarms[i].particles[j].cost = MultiSwarmProgram.Cost(swarms[i].particles[j].position);

                        // check if new best cost
                        if (swarms[i].particles[j].cost < swarms[i].particles[j].bestPartCost)
                        {
                            swarms[i].particles[j].bestPartCost = swarms[i].particles[j].cost;
                            Array.Copy(swarms[i].particles[j].position, swarms[i].particles[j].bestPartPos, dim);
                        }

                        if (swarms[i].particles[j].cost < swarms[i].bestSwarmCost)
                        {
                            swarms[i].bestSwarmCost = swarms[i].particles[j].cost;
                            Array.Copy(swarms[i].particles[j].position, swarms[i].bestSwarmPos, dim);
                        }

                        if (swarms[i].particles[j].cost < bestMultiCost)
                        {
                            bestMultiCost = swarms[i].particles[j].cost;
                            Array.Copy(swarms[i].particles[j].position, bestMultiPos, dim);
                        }
                    }
                }

            }
        }

        private void Immigration(int i, int j)
        {
            // swap particle j in swarm i, with a random particle in a random swarm
            int otheri = MultiSwarmProgram.ran.Next(0, swarms.Length);
            int otherj = MultiSwarmProgram.ran.Next(0, swarms[0].particles.Length);
            Particle tmp = swarms[i].particles[j];
            swarms[i].particles[j] = swarms[otheri].particles[otherj];
            swarms[otheri].particles[otherj] = tmp;
        }

        public override string ToString()
        {
            string s = "";
            s += "=======================\n";
            for (int i = 0; i < swarms.Length; ++i)
                s += swarms[i].ToString() + "\n";
            s += "Best Multiswarm Pos [ ";
            for (int i = 0; i < bestMultiPos.Length; ++i)
                s += bestMultiPos[i].ToString("F2") + " ";
            s += "] ";
            s += "Best Multiswarm Cost = " + bestMultiCost.ToString("F3");
            s += "\n=======================\n";
            return s;
        }

    } // Multiswarm

} // ns
