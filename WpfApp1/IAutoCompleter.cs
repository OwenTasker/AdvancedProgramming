using System.Collections.Generic;

namespace WpfApp1
{
    public interface IAutoCompleter
    {
        public void InitialiseSuggestions();

        public void UpdateSuggestions();

        public IEnumerable<string> GetMatches(string partialMatch);
    }
}