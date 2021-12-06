using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace WpfApp1
{
    public class AutoCompleter : IAutoCompleter
    {

        private readonly IInterpreter _interpreter;
        private readonly IPrefixTree _prefixTree;

        public AutoCompleter(IInterpreter interpreter, IPrefixTree prefixTree)
        {
            _interpreter = interpreter;
            _prefixTree = prefixTree;
        }

        public void InitialiseSuggestions()
        {
            _prefixTree.Clear();
            foreach (var (function, description) in _interpreter.GetFunctions())
            {
                _prefixTree.Add(function + " : " + description);
            }
        }

        public void UpdateSuggestions()
        {
            InitialiseSuggestions();

            foreach (var (key, _) in _interpreter.GetVariables().ToList())
            {
                _prefixTree.Add(key);
            }
        }

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