using System.Collections.Generic;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    public interface IInterpreter
    {
        public IDictionary<string, FSharpList<Util.terminal>> Environment { get; set; }

        public string Interpret(string command);

        public string Interpret(string command, IDictionary<string, FSharpList<Util.terminal>> environment);

        public IEnumerable<KeyValuePair<string, string>> GetVariables();

        public IDictionary<string, FSharpList<Util.terminal>> GetTempEnvironment(string assignment);

        public void ClearEnvironment();

        public IEnumerable<string> GetOpenVariables(string expression);

        public FSharpList<Util.terminal> GetTerminalListFromString(string input);

        public string GetStringFromTerminal(Util.terminal terminal);
    }
}