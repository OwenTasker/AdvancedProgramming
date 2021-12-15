using System.Collections.Generic;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    /// <summary>
    /// Interface to represent the contract to be fulfilled for an interpreter MyMathsPal.
    /// </summary>
    public interface IInterpreter
    {
        /// <summary>
        /// Property representing the current execution environment for the interpreter.
        /// </summary>
        public IDictionary<string, FSharpList<Util.terminal>> Environment { get; set; }

        /// <summary>
        /// Method to interpret a command given as a string.
        /// </summary>
        /// <param name="command">A string representing the command to be interpreted.</param>
        /// <returns>A string representation of the result of interpretation.</returns>
        public string Interpret(string command);

        /// <summary>
        /// Method to retrieve all of the currently declared variables in the environment.
        /// </summary>
        /// <returns>A list of the variables in the execution environment and the assignments.</returns>
        public IEnumerable<KeyValuePair<string, string>> GetVariables();

        /// <summary>
        /// Method to reset the execution environment.
        /// </summary>
        public void ClearEnvironment();

        /// <summary>
        /// Method to transform a given string into its terminal representation.
        /// </summary>
        /// <param name="input">The string to be transformed.</param>
        /// <returns>A list of terminals representing the string.</returns>
        public FSharpList<Util.terminal> GetTerminalListFromString(string input);

        /// <summary>
        /// Method to retrieve the string value of a given terminal.
        /// </summary>
        /// <param name="terminal">The terminal to convert.</param>
        /// <returns>A string representation of the terminal.</returns>
        public string GetStringFromTerminal(Util.terminal terminal);

        /// <summary>
        /// Method to get the list of system functions for this interpreter.
        /// </summary>
        /// <returns></returns>
        public IEnumerable<System.Tuple<string, string>> GetFunctions();

        /// <summary>
        /// Method to retrieve two lists of doubles based on a given input statement.
        /// </summary>
        /// <param name="statement">The statement to be used to generate the arrays.s</param>
        /// <returns>Two arrays based on the input statement.</returns>
        public (List<double>, List<double>, string) GetXyValues(string statement);
    }
}