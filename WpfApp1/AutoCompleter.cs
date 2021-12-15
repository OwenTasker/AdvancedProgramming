using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace WpfApp1
{
    /// <summary>
    /// Implementation of an autocompleter using a prefix tree.
    /// </summary>
    public class AutoCompleter : IAutoCompleter
    {
        /// <summary>
        /// The interpreter for this session.
        /// </summary>
        private readonly IInterpreter _interpreter;

        /// <summary>
        /// The prefix tree in which to check for matches.
        /// </summary>
        private readonly IPrefixTree _prefixTree;

        /// <summary>
        /// Construct a prefix tree given an interpreter and a prefix tree.
        /// </summary>
        /// <param name="interpreter">The interpreter for this autocompleter.</param>
        /// <param name="prefixTree">The prefix tree for this autocompleter.</param>
        public AutoCompleter(IInterpreter interpreter, IPrefixTree prefixTree)
        {
            _interpreter = interpreter;
            _prefixTree = prefixTree;
        }

        /// <summary>
        /// Method to initialise the values in the prefix tree based upon the interpreter.
        /// </summary>
        public void InitialiseSuggestions()
        {
            _prefixTree.Clear();
            foreach (var (function, description) in _interpreter.GetFunctions())
            {
                _prefixTree.Add(function + " : " + description);
            }
        }

        /// <summary>
        /// Method to update the current suggestions given based on updates to the interpreter.
        /// </summary>
        public void UpdateSuggestions()
        {
            InitialiseSuggestions();

            foreach (var (key, _) in _interpreter.GetVariables().ToList())
            {
                _prefixTree.Add(key);
            }
        }

        /// <summary>
        /// Method to retrieve all matches in the prefix tree for a given input string.
        /// </summary>
        /// <param name="input">The string for which to check matches.</param>
        /// <returns>A list of matches contained within the prefix tree.</returns>
        public IEnumerable<string> GetMatches(string input)
        {
            var split = Regex.Split(input, @"[^a-zA-Z]");

            var partialMatch = "";

            for (var i = split.Length - 1; i >= 0; i--)
            {
                if (!split[i].Equals(""))
                {
                    partialMatch = split[i];
                    break;
                }
            }

            return _prefixTree.Contains(partialMatch);
        }
    }
}