
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
            // var rawInput = Console.ReadLine();
            var rawInput = "1*100+1234121";
            Console.WriteLine("You Wrote: " + rawInput);

            //Take an input and implicitly turn it into a List of strings
            var inputList = rawInput.Select(c => c.ToString()).ToList();
            
            //Need to turn Generic System.List to fSharpList
            var fsharpList = ListModule.OfSeq(inputList);
          
            var tokenizedInput = Lexer.lex(fsharpList);
            var scannedInput = Lexer.scan(tokenizedInput, FSharpList<Util.terminal>.Empty);
            

        }
    }
}
