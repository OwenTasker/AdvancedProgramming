using System;
using System.Collections.Generic;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
using Ninject.Infrastructure.Language;

namespace WpfApp1
{
    /// <summary>
    /// A class to fulfill interpretation in MyMathsPal by interfacing with an F# interpreter.
    /// </summary>
    public class Interpreter : IInterpreter
    {
        /// <summary>
        /// Property representing the execution environment for this interpreter.
        /// </summary>
        public IDictionary<string, FSharpList<Util.terminal>> Environment { get; set; } =
            new Dictionary<string, FSharpList<Util.terminal>>();

        /// <summary>
        /// Method to control interpretation of a given user input command.
        /// </summary>
        /// <param name="command">The user input to be interpreted.</param>
        /// <returns>A string representing the result of interpretation.</returns>
        public string Interpret(string command)
        {
            var inputList = command.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            var (item1, item2) = PublicInterface.interpret(inputFSharpList, PublicInterface.toMap(Environment));

            Environment = item2;

            return PublicInterface.terminalListToString(item1);
        }

        /// <summary>
        /// Method to retrieve the variables currently contained within the execution environment.
        /// </summary>
        /// <returns>A list of variables and their assignments, both as strings.</returns>
        public IEnumerable<KeyValuePair<string, string>> GetVariables()
        {
            return Environment.ToList().Select(a =>
                new KeyValuePair<string, string>(a.Key, PublicInterface.terminalListToString(a.Value)));
        }

        /// <summary>
        /// Method to reset the current execution environment.
        /// </summary>
        public void ClearEnvironment()
        {
            Environment = new Dictionary<string, FSharpList<Util.terminal>>();
        }

        /// <summary>
        /// Method to retrieve a string representation of a given terminal.
        /// </summary>
        /// <param name="terminal">The terminal to transform.</param>
        /// <returns>A string representing the terminal.</returns>
        public string GetStringFromTerminal(Util.terminal terminal)
        {
            var inputList = new List<Util.terminal> {terminal};
            return PublicInterface.terminalListToString(ListModule.OfSeq(inputList));
        }

        /// <summary>
        /// Method to retrieve the system functions recognised by this interpreter.
        /// </summary>
        /// <returns>A list of system functions and their description, as strings.</returns>
        public IEnumerable<Tuple<string, string>> GetFunctions()
        {
            return Util.functions.ToEnumerable();
        }

        /// <summary>
        /// Method to retrieve x and y lists based on a given input statement.
        /// </summary>
        /// <param name="statement">The statement to be used for generating the lists.</param>
        /// <returns>A list of doubles for x and a list of doubles for y given the input command.</returns>
        public (List<double>, List<double>, string) GetXyValues(string statement)
        {
            var inputList = statement.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            var (x, y, command) = PublicInterface.getXYValues(inputFSharpList);

            return (x.ToList(), y.ToList(), PublicInterface.terminalListToString(command));
        }

        /// <summary>
        /// Method to retrieve the terminal representation of a given input string.
        /// </summary>
        /// <param name="input">The input to be converted.</param>
        /// <returns>A terminal list representing the input string.</returns>
        public FSharpList<Util.terminal> GetTerminalListFromString(string input)
        {
            var inputList = input.Select(c => c.ToString()).ToList();
            var inputFSharpList = ListModule.OfSeq(inputList);

            return PublicInterface.stringToTerminalList(inputFSharpList);
        }


    }
}