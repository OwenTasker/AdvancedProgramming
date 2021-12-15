namespace WpfApp1
{
    /// <summary>
    /// Implementation of a graph data calculator in MyMathsPal.
    /// </summary>
    public class GraphDataCalculator : IGraphDataCalculator
    {
        /// <summary>
        /// The interpreter to be used by this graph data calculator for calculating points.
        /// </summary>
        private readonly IInterpreter _interpreter;

        /// <summary>
        /// Constructor, initialises the interpreter to the given input.
        /// </summary>
        /// <param name="interpreter">The interpreter to be used by this graph data calculator.</param>
        public GraphDataCalculator(IInterpreter interpreter)
        {
            _interpreter = interpreter;
        }

        /// <summary>
        /// Retrieves two arrays from the interpreter by providing an input command to generate them.
        /// </summary>
        /// <param name="command">The user input the generate the arrays.</param>
        /// <returns>Two arrays of doubles based upon the input string.</returns>
        public (double[], double[], string) GetXyArrays(string command)
        {
            var (xList, yList, function) = _interpreter.GetXyValues(command);
            return (xList.ToArray(), yList.ToArray(), function);
        }
    }
}