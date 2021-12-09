using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Interpreter;
using Microsoft.FSharp.Collections;
using Ninject.Infrastructure.Language;

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

            var (item1, item2) = PublicInterface.interpret(inputFSharpList, PublicInterface.toMap(Environment));

            Environment = item2;

            return PublicInterface.terminalListToString(item1);
        }

        public string Interpret(string command, IDictionary<string, FSharpList<Util.terminal>> environment)
        {
            var inputList = command.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            var (item1, _) = PublicInterface.interpret(inputFSharpList, PublicInterface.toMap(environment));

            return PublicInterface.terminalListToString(item1);
        }

        public IEnumerable<KeyValuePair<string, string>> GetVariables()
        {
            return Environment.ToList().Select(a =>
                new KeyValuePair<string, string>(a.Key, PublicInterface.terminalListToString(a.Value)));
        }

        public IDictionary<string, FSharpList<Util.terminal>> GetTempEnvironment(string assignment)
        {
            var inputList = assignment.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return PublicInterface.interpret(inputFSharpList, PublicInterface.toMap(Environment)).Item2;
        }

        public void ClearEnvironment()
        {
            Environment = new Dictionary<string, FSharpList<Util.terminal>>();
        }

        private bool Closed(string variable)
        {
            var inputList = variable.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return PublicInterface.closed(inputFSharpList, PublicInterface.toMap(Environment));
        }

        public IEnumerable<string> GetOpenVariables(string expression)
        {
            var variables = Regex.Replace(
                expression, "[^a-zA-Z]", "|").Split("|").Where(
                s => s.Length > 0).ToList();

            var systemFunctions = Util.functions.Select(x => x.Item1).ToList();
            variables = variables.Except(systemFunctions).ToList();

            // Compile set of open variables
            var openVars = new List<string> {variables.ToArray()[0]};

            foreach (var t in variables.ToArray()[1..])
            {
                if (!(Closed(t) || openVars.Contains(t)))
                    openVars.Add(t);
            }

            return openVars;
        }

        public string GetStringFromTerminal(Util.terminal terminal)
        {
            var inputList = new List<Util.terminal> {terminal};
            return PublicInterface.terminalListToString(ListModule.OfSeq(inputList));
        }

        public IEnumerable<Tuple<string, string>> GetFunctions()
        {
            return Util.functions.ToEnumerable();
        }

        public (List<double>, List<double>, string) GetXyValues(string statement)
        {
            var inputList = statement.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            var (x, y, command) = PublicInterface.getXYValues(inputFSharpList);

            return (x.ToList(), y.ToList(), PublicInterface.terminalListToString(command));
        }

        public FSharpList<Util.terminal> GetTerminalListFromString(string input)
        {
            var inputList = input.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return PublicInterface.stringToTerminalList(inputFSharpList);
        }


    }
}