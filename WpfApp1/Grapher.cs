using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    public class GraphDataCalculator : IGraphDataCalculator
    {
        
        /// <summary>
        /// Method to create execution environment including variables held in f sharp.
        /// </summary>
        private static IDictionary<string, FSharpList<Util.terminal>> CreateExecutionEnvironment(string function, IDictionary<string, FSharpList<Util.terminal>> environment)
        {
            var funcStrings = function.Select(s => s.ToString()).ToList();
            
            var funcAsFSharpList = ListModule.OfSeq(funcStrings);
            var lexerOutput = Lexer.lexer(funcAsFSharpList);
            
            Parser.parse(lexerOutput);

            var (_, item2) = Exec.exec(lexerOutput,
                Util.toMap(new Dictionary<string, FSharpList<Util.terminal>>()));

            var tempDict = item2.ToList().ToDictionary(
                pair => pair.Key, pair => pair.Value);

            environment.ToList().ForEach(x => tempDict.Add(x.Key, x.Value));
            
            return tempDict;
        }

        /// <summary>
        /// Method to compute all y values based on function and calculated x values.
        /// </summary>
        public static double[] ComputeYArray(IReadOnlyList<string> trimmedArgsArray, IReadOnlyList<double> xArray, IDictionary<string, FSharpList<Util.terminal>> environment)
        {
            var yArray = new double[750];

            // Get variables from function
            var openVars = GetOpenVariables(trimmedArgsArray[0], environment);

            var executionEnvironment = CreateExecutionEnvironment(trimmedArgsArray[0], environment);

            for (var i = 0; i < 750; i++)
            {
                string query;
                if (openVars.Count < 2)
                {
                    query = openVars[0] + "()";
                }
                else
                {
                    query = openVars[0] + "(" + openVars[1] + "->" + xArray[i] + ")";
                }
                
                var queryList = query.Select(c => c.ToString()).ToList();
                var inputFSharpList = ListModule.OfSeq(queryList);
                var lexedQuery = Lexer.lexer(inputFSharpList);

                var (executedQuery, _) = Exec.exec(lexedQuery, Util.toMap(executionEnvironment));

                yArray[i] = double.Parse(Util.terminalListToString("", executedQuery));
            }

            return yArray;
        }

        /// <summary>
        /// Method to calculate x values evenly spaced between min and max.
        /// </summary>
        public static double[] ComputeXArray(IReadOnlyList<string> trimmedArgsArray)
        {
            var min = double.Parse(trimmedArgsArray[1]);
            var max = double.Parse(trimmedArgsArray[2]);
                
            if (min > max)
            {
                throw new Util.ExecError();
            }
            var range = max - min;
            var step = range / 749.0;
            
            var xArray = new double[750];
            for (var i = 0; i < 750; i++)
            {
                xArray[i] = min + (i) * step;
            }

            return xArray;
        }

        /// <summary>
        /// Method to check if used variables are free.
        /// </summary>
        private static List<string> GetOpenVariables(string function, IDictionary<string, FSharpList<Util.terminal>> environment)
        {
            var variables = Regex.Replace(
                function, "[^a-zA-Z]", "|").Split("|").Where(
                s => s.Length > 0).ToArray();

            // Compile set of open variables
            var openVars = new List<string>();

            var count = 0;

            foreach (var t in variables)
            {
                var fSharpList = FSharpList<string>.Empty;
                var enumerable = fSharpList.Append(t);
                var lexed = Lexer.lexer(ListModule.OfSeq(enumerable));

                if (count > 0)
                    if (Exec.closed(lexed, Util.toMap(environment)) || openVars.Contains(t)) 
                        continue;

                openVars.Add(t);
                count++;
            }

            if (openVars.Count > 2)
            {
                throw new Util.ExecError();
            }

            return openVars;
        }

        /// <summary>
        /// Method to clean and validate form of user input.
        /// </summary>
        public static List<string> TrimmedArgsArray(string input)
        {
            // Get plot parameters as an array
            var args = input[5..^1];
            var argsArray = args.Split(",");
            var trimmedArgsArray = argsArray.ToList().Select(x => x.Trim()).ToList();

            //Set default range if not supplied
            if (trimmedArgsArray.Count == 1)
            {
                var tempArray = trimmedArgsArray;
                trimmedArgsArray = new List<string>
                {
                    tempArray.First(),
                    "-10",
                    "10"
                };
            }
            
            // If invalid number of parameters passed then throw error
            if (trimmedArgsArray.Count != 3)
            {
                // Think about maybe just presenting a message instead
                throw new Util.ExecError();
            }

            //If range is backwards, swap them
            if (Convert.ToDouble(trimmedArgsArray[1]) > Convert.ToDouble(trimmedArgsArray[2]))
            {
                (trimmedArgsArray[1], trimmedArgsArray[2]) = (trimmedArgsArray[2], trimmedArgsArray[1]);
            }
            
            //If range is 0, change to default range
            if (trimmedArgsArray[1] == trimmedArgsArray[2])
            {
                trimmedArgsArray[1] = "-10";
                trimmedArgsArray[2] = "10";
            }

            return trimmedArgsArray;
        }
    }
}