using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    public class Interpreter : IInterpreter
    {
        public IDictionary<string, FSharpList<Util.terminal>> Environment { get; set; } =
            new Dictionary<string, FSharpList<Util.terminal>>();

        public string Interpret(string command)
        {
            var inputList = command.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            var (item1, item2) = Program.interpret(inputFSharpList, Util.toMap(Environment));

            Environment = item2;

            return Util.terminalListToString("", item1);
        }

        public string Interpret(string command, IDictionary<string, FSharpList<Util.terminal>> environment)
        {
            var inputList = command.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            var (item1, _) = Program.interpret(inputFSharpList, Util.toMap(environment));

            return Util.terminalListToString("", item1);
        }

        public IEnumerable<KeyValuePair<string, string>> GetVariables()
        {
            return Environment.ToList().Select(a =>
                new KeyValuePair<string, string>(a.Key, Util.terminalListToString("", a.Value)));
        }

        public IDictionary<string, FSharpList<Util.terminal>> GetTempEnvironment(string assignment)
        {
            var inputList = assignment.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return Program.interpret(inputFSharpList, Util.toMap(Environment)).Item2;
        }

        public void ClearEnvironment()
        {
            Environment = new Dictionary<string, FSharpList<Util.terminal>>();
        }

        private bool Closed(string variable)
        {
            var inputList = variable.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return Program.closed(inputFSharpList, Util.toMap(Environment)).Item1;
        }

        public IEnumerable<string> GetOpenVariables(string expression)
        {
            var variables = Regex.Replace(
                expression, "[^a-zA-Z]", "|").Split("|").Where(
                s => s.Length > 0).ToArray();

            // Compile set of open variables
            var openVars = new List<string> {variables[0]};

            foreach (var t in variables[1..])
            {
                if (!(Closed(t) || openVars.Contains(t)))
                    openVars.Add(t);
            }

            return openVars;
        }

        public string GetStringFromTerminal(Util.terminal terminal)
        {
            var inputList = new List<Util.terminal> {terminal};
            return Util.terminalListToString("", ListModule.OfSeq(inputList));
        }

        public FSharpList<Util.terminal> GetTerminalListFromString(string input)
        {
            var inputList = input.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return Program.getTerminalListFromString(inputFSharpList);
        }
    }
}