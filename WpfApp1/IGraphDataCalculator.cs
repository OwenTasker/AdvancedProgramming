namespace WpfApp1
{
    public interface IGraphDataCalculator
    {
        public (double[], double[], string) GetXyArrays(string command);
    }
}