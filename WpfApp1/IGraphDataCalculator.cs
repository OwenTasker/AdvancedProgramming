using System.Collections.Generic;

namespace WpfApp1
{
    public interface IGraphDataCalculator
    {
        public double[] ComputeYArray(IReadOnlyList<string> trimmedArgsArray, IReadOnlyList<double> xArray,
            IInterpreter interpreter);

        public double[] ComputeXArray(double min, double max);

        public List<string> TrimmedArgsArray(string input);
    }
}