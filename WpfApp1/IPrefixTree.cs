using System.Collections.Generic;

namespace WpfApp1
{
    /// <summary>
    /// Interface to represent the contract to be fulfilled for a prefix tree in MyMathsPal.
    /// </summary>
    public interface IPrefixTree
    {
        /// <summary>
        /// Method to clear the prefix tree.
        /// </summary>
        public void Clear();

        /// <summary>
        /// Method to add an item to the prefix tree.
        /// </summary>
        /// <param name="item">The item to be added.</param>
        public void Add(string item);

        /// <summary>
        /// Method to retrieve a list of matches based on a given prefix string.
        /// </summary>
        /// <param name="partialMatch">The string for which to check matches.</param>
        /// <returns>A list of matches in the prefix tree.</returns>
        public IEnumerable<string> Contains(string partialMatch);
    }
}