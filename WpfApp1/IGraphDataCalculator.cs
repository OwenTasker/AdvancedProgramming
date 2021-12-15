namespace WpfApp1
{
    /// <summary>
    /// Interface to represent the contract to be fulfilled for a graph data calculator in MyMathsPal.
    /// </summary>
    public interface IGraphDataCalculator
    {
        /// <summary>
        /// Method to get x and y arrays given an input command.
        /// </summary>
        /// <param name="command">A string representing a command to generate the arrays</param>
        /// <returns>Two arrays of doubles, to be used for the x and y axis.</returns>
        public (double[], double[], string) GetXyArrays(string command);
    }
}