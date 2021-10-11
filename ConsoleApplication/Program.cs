
using System;
using System.Collections.Generic;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace ConsoleApplication
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Enter an arithmetic operation: \n >> ");;
            var rawInput = Console.ReadLine();
            Console.WriteLine("You Wrote: " + rawInput);

            var inputList = new List<string>();

            //Turn the input string into a list of strings for each char
            foreach (char c in rawInput)
            {
                inputList.Add(c.ToString());
            }
            
            //Need to turn Generic System.List to fSharpList
            var fsharpList = ListModule.OfSeq(inputList);
            
            //bug   If last value entered is an int type program will error with an ArgumentException, it looks like
            //      there is something funky going on with how F# interacts with C# -> F# Lists, if a non-int type
            //      is the last value - the issue does not exist
            var tokenizedInput = Interpreter.Lexer.lex(fsharpList);
            var scannedInput = Interpreter.Lexer.scan(tokenizedInput, FSharpList<Util.terminal>.Empty);
            foreach (var VARIABLE in scannedInput)
            {
                Console.WriteLine(VARIABLE);
            }
            

        }
    }
}
