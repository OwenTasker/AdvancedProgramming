
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
            var inputList = ListModule.OfSeq(rawInput.Select(c => c.ToString()).ToList());
            
            Interpreter.Program.interpret(inputList, Util.toMap(new Dictionary<string, FSharpList<Util.terminal>>()));
            
        }
    }
}
