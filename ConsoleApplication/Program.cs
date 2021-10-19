
using System;
using System.Collections.Generic;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace ConsoleApplication
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Enter an arithmetic operation: \n >> ");
            var rawInput = Console.ReadLine();
            Console.WriteLine("You Wrote: " + rawInput);

            //Take an input and implicitly turn it into a List of strings
            var inputList = rawInput.Select(c => c.ToString()).ToList();
            
            //Need to turn Generic System.List to fSharpList
            var fSharpList = ListModule.OfSeq(inputList);

            var tokenizedInput = Lexer.lexer(fSharpList);
            // var res = Exec.reduce(tokenizedInput.Item2);
            // Console.WriteLine(res);

            foreach (var VARIABLE in tokenizedInput.Item2)
            {
                Console.WriteLine(VARIABLE);
            }
        }
    }
}
