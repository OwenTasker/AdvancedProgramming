using System.Collections.Generic;

namespace WpfApp1
{
    /// <summary>
    /// Interface to represent the contract to be fulfilled for an autocompleter in MyMathsPal.
    /// </summary>
    public interface IAutoCompleter
    {
        /// <summary>
        /// Method to initialise the list of suggestions.
        /// </summary>
        public void InitialiseSuggestions();

        /// <summary>
        /// Method to check for and enact any updates to the suggestions.
        /// </summary>
        public void UpdateSuggestions();

        /// <summary>
        /// Method to find the current matches in the suggestion list of a given prefix string.
        /// </summary>
        /// <param name="partialMatch">The string for which to check matches.</param>
        /// <returns>The strings in the suggestion list containing the partial match</returns>
        public IEnumerable<string> GetMatches(string partialMatch);
    }
}