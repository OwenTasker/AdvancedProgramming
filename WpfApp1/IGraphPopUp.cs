namespace WpfApp1
{
    /// <summary>
    /// Interface to represent the contract to be fulfilled for a graph pop up in MyMathsPal.
    /// </summary>
    public interface IGraphPopUp
    {
        public void GenerateGraph(string input);
        public void ClosePopUp();
    }
}