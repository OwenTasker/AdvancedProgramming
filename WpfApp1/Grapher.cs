using System;
using System.Collections.Generic;
using System.Linq;
using Interpreter;

namespace WpfApp1
{
    public class GraphDataCalculator : IGraphDataCalculator
    {
        /// <summary>
        /// Method to compute all y values based on function and calculated x values.
        /// </summary>
        public double[] ComputeYArray(IReadOnlyList<string> trimmedArgsArray, IReadOnlyList<double> xArray, IInterpreter interpreter)
        {
            var yArray = new double[750];

            // Get variables from function
            var openVars = interpreter.GetOpenVariables(trimmedArgsArray[0]).ToList();
            
            if (openVars.Count > 2)
            {
                throw new Util.ExecError();
            }

            var executionEnvironment = interpreter.GetTempEnvironment(trimmedArgsArray[0]);

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

                var executedQuery = interpreter.Interpret(query, executionEnvironment);

                yArray[i] = double.Parse(executedQuery);
            }

            return yArray;
        }

        /// <summary>
        /// Method to calculate x values evenly spaced between min and max.
        /// </summary>
        public double[] ComputeXArray(double min, double max)
        {
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
        /// Method to clean and validate form of user input.
        /// </summary>
        public List<string> TrimmedArgsArray(string input)
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