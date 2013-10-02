// original code
// downloaded here: http://archive.msdn.microsoft.com/mag201310TestRun

using System;
namespace RadialNetworksInputOutput
{
  public class RadialNetIOProgram
  {
    public static void Main(string[] args)
    {
      Console.WriteLine("\nBegin Radial Basis Function (RBF) network input-output demo\n");

      int numInput = 3;
      int numHidden = 4;
      int numOutput = 2;

      Console.WriteLine("\nCreating a 3-4-2 radial net");
      RadialNet rn = new RadialNet(numInput, numHidden, numOutput);

      double[][] centroids = new double[4][];
      centroids[0] = new double[] { -3.0, -3.5, -3.8 };
      centroids[1] = new double[] { -1.0, -1.5, -1.8 };
      centroids[2] = new double[] { 2.0, 2.5, 2.8 };
      centroids[3] = new double[] { 4.0, 4.5, 4.8 };
      Console.WriteLine("\nSetting centroids (means) to:");
      Helpers.ShowMatrix(centroids, -1, 2);
      Console.WriteLine("Loading centroids into radial net");
      rn.SetCentroids(centroids);

      double[] stdDevs = new double[4] { 2.22, 3.33, 4.44, 5.55 };
      Console.WriteLine("\nSetting standard deviations (widths) to:");
      Helpers.ShowVector(stdDevs, 2, 4, true);
      Console.WriteLine("Loading standard deviations into radial net");
      rn.SetStdDevs(stdDevs);

      double[][] hoWeights = new double[4][];
      hoWeights[0] = new double[2] { 5.0, -5.1 };
      hoWeights[1] = new double[2] { -5.2, 5.3 };
      hoWeights[2] = new double[2] { -5.4, 5.5 };
      hoWeights[3] = new double[2] { 5.6, -5.7 };
      Console.WriteLine("\nSetting hidden-output weights to:");
      Helpers.ShowMatrix(hoWeights, -1, 2);
      Console.WriteLine("Loading hidden-output weights into radial net");
      rn.SetWeights(hoWeights);

      double[] oBiases = new double[2] { 7.0, 7.1 };
      Console.WriteLine("\nSetting output biases to:");
      Helpers.ShowVector(oBiases, 1, 4, true);
      Console.WriteLine("Loading output biases into radial net");
      rn.SetBiases(oBiases);

      double[] xValues = new double[3] { 1.0, -2.0, 3.0 };
      Console.WriteLine("\nSetting x-input to:");
      Helpers.ShowVector(xValues, 1, 4, true);

      Console.WriteLine("\nComputing the output of the radial net\n");
      double[] yValues = rn.ComputeOutputs(xValues);

      Console.WriteLine("\nThe output of the RBF network is:");
      Helpers.ShowVector(yValues, 4, 4, true);

      Console.WriteLine("\nEnd RBF network demo\n");
      Console.ReadLine();
    } // Main

  } // Program

  public class RadialNet
  {
    private int numInput;
    private int numHidden;
    private int numOutput;

    private double[] inputs;
    private double[][] centroids; // aka means
    private double[] stdDevs; // aka widths
    
    private double[][] hoWeights;
    private double[] oBiases;
    private double[] outputs;

    // ---------------------------------------------------------

    public RadialNet(int numInput, int numHidden, int numOutput)
    {
      this.numInput = numInput;
      this.numHidden = numHidden;
      this.numOutput = numOutput;

      this.inputs = new double[numInput];
      this.centroids = MakeMatrix(numHidden, numInput);
      this.stdDevs = new double[numHidden];

      this.hoWeights = MakeMatrix(numHidden, numOutput);
      this.oBiases = new double[numOutput];
      this.outputs = new double[numOutput];
    } // ctor

    private static double[][] MakeMatrix(int rows, int cols)
    {
      double[][] result = new double[rows][];
      for (int r = 0; r < rows; ++r)
        result[r] = new double[cols];
      return result;
    }

    // ---------------------------------------------------------

    public void SetCentroids(double[][] centroids)
    {
      if (centroids.Length != numHidden)
        throw new Exception("Bad number of centroids");
      if (centroids[0].Length != numInput)
        throw new Exception("Bad centroid size");

      for (int i = 0; i < numHidden; ++i)
        for (int j = 0; j < numInput; ++j)
          this.centroids[i][j] = centroids[i][j]; 
    }

    public void SetStdDevs(double[] stdDevs)
    {
      if (stdDevs.Length != numHidden)
        throw new Exception("Bad number of stdDevs");
      Array.Copy(stdDevs, this.stdDevs, stdDevs.Length);
    }

    public void SetWeights(double[][] hoWeights)
    {
      if (hoWeights.Length != numHidden)
        throw new Exception("Bad number of weights");
      if (hoWeights[0].Length != numOutput)
        throw new Exception("Bad number of weights");
      for (int i = 0; i < numHidden; ++i)
        for (int j = 0; j < numOutput; ++j)
          this.hoWeights[i][j] = hoWeights[i][j]; 
    }

    public void SetBiases(double[] oBiases)
    {
      if (oBiases.Length != numOutput)
        throw new Exception("Bad number of hoBiases");
      Array.Copy(oBiases, this.oBiases, oBiases.Length);
    }

    // ---------------------------------------------------------

    public double[] ComputeOutputs(double[] xValues)
    {
      Array.Copy(xValues, inputs, xValues.Length);

      double[] hOutputs = new double[numHidden]; // hidden node outputs
      for (int j = 0; j < numHidden; ++j) // each hidden node
      {
        double d = Distance(inputs, centroids[j]); // could use a 'distSquared' approach
        Console.WriteLine("\nHidden[" + j + "] distance = " + d.ToString("F4"));
        double r = -1.0 * (d * d) / (2 * stdDevs[j] * stdDevs[j]);
        Console.WriteLine("\nHidden[" + j + "] r = " + r.ToString("F4"));
        double g = Math.Exp(r);
        Console.WriteLine("Hidden[" + j + "] output = " + g.ToString("F4"));
        hOutputs[j] = g;
      }

      for (int k = 0; k < numOutput; ++k)
        outputs[k] = 0.0;

      for (int k = 0; k < numOutput; ++k)
        for (int j = 0; j < numHidden; ++j)
          outputs[k] += (hOutputs[j] * hoWeights[j][k]);

      for (int k = 0; k < numOutput; ++k)
        outputs[k] += oBiases[k];

      double[] result = new double[numOutput];
      Array.Copy(outputs, result, outputs.Length);
      return result;
    }

    public static double Distance(double[] x, double[] c)
    {
      // distance between x vector and centroid
      double sum = 0.0;
      for (int i = 0; i < x.Length; ++i)
      {
          sum += (x[i] - c[i]) * (x[i] - c[i]);
      }
      return Math.Sqrt(sum);
    }

    // ---------------------------------------------------------

  } // RadialNet

  // ===========================================================================

  public class Helpers
  {
    public static void ShowVector(double[] vector, int decimals, int valsPerLine, bool blankLine)
    {
      for (int i = 0; i < vector.Length; ++i)
      {
        if (i > 0 && i % valsPerLine == 0) // max of 12 values per row 
          Console.WriteLine("");
        if (vector[i] >= 0.0) Console.Write(" ");
        Console.Write(vector[i].ToString("F" + decimals) + " "); // n decimals
      }
      if (blankLine) Console.WriteLine("\n");
    }

    public static void ShowVector(int[] vector, int valsPerLine, bool blankLine)
    {
      for (int i = 0; i < vector.Length; ++i)
      {
        if (i > 0 && i % valsPerLine == 0) // max of 12 values per row 
          Console.WriteLine("");
        if (vector[i] >= 0.0) Console.Write(" ");
        Console.Write(vector[i] + " ");
      }
      if (blankLine) Console.WriteLine("\n");
    }

    public static void ShowMatrix(double[][] matrix, int numRows, int decimals)
    {
      int ct = 0;
      if (numRows == -1) numRows = int.MaxValue; // if numRows == -1, show all rows
      for (int i = 0; i < matrix.Length && ct < numRows; ++i)
      {
        for (int j = 0; j < matrix[0].Length; ++j)
        {
          if (matrix[i][j] >= 0.0) Console.Write(" "); // blank space instead of '+' sign
          Console.Write(matrix[i][j].ToString("F" + decimals) + " ");
        }
        Console.WriteLine("");
        ++ct;
      }
      Console.WriteLine("");
    }
  } // class Helpers

} // ns
