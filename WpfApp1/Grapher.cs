using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    public class Grapher
    {
        
        /// <summary>
        /// Execution environment for this session.
        /// </summary>
        private static IDictionary<string, FSharpList<Util.terminal>> _environment =
            new Dictionary<string, FSharpList<Util.terminal>>();
        
        private static IDictionary<string, FSharpList<Util.terminal>> CreateExecutionEnvironment(string function)
        {
            var funcStrings = function.Select(s => s.ToString()).ToList();
            
            var funcAsFSharpList = ListModule.OfSeq(funcStrings);
            var lexerOutput = Lexer.lexer(funcAsFSharpList);
            
            Parser.expression(lexerOutput);

            
            
            var (_, item2) = Exec.exec(lexerOutput,
                Util.toMap(new Dictionary<string, FSharpList<Util.terminal>>()));

            var tempDict = item2.ToList().ToDictionary(
                pair => pair.Key, pair => pair.Value);

            _environment.ToList().ForEach(x => tempDict.Add(x.Key, x.Value));
            
            return tempDict;
        }

        public static double[] ComputeYArray(IReadOnlyList<string> trimmedArgsArray, IReadOnlyList<double> xArray)
        {
            var yArray = new double[750];

            // Get variables from function
            var openVars = GetOpenVariables(trimmedArgsArray[0]);

            var environment = CreateExecutionEnvironment(trimmedArgsArray[0]);

            for (var i = 0; i < 750; i++)
            {
                string query;
                if (openVars.Count == 1)
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

                var (executedQuery, _) = Exec.exec(lexedQuery, Util.toMap(environment));

                yArray[i] = double.Parse(Util.terminalListToString("", executedQuery));
            }

            return yArray;
        }

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

        private static List<string> GetOpenVariables(string function)
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
                    if (Exec.closed(lexed, Util.toMap(_environment)) || openVars.Contains(t)) 
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

            return trimmedArgsArray;
        }
    }
}