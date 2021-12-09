namespace WpfApp1
{
    public class GraphDataCalculator : IGraphDataCalculator
    {

        private readonly IInterpreter _interpreter;

        public GraphDataCalculator(IInterpreter interpreter)
        {
            _interpreter = interpreter;
        }

        public (double[], double[], string) GetXyArrays(string command)
        {
            var (xList, yList, function) = _interpreter.GetXyValues(command);
            return (xList.ToArray(), yList.ToArray(), function);
        }
    }
}