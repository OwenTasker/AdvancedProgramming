
using System.Collections.Generic;

namespace ConsoleApplication
{
    class Program
    {
        static void Main(string[] args)
        {
            System.Console.Write("Enter an arithmetic operation: \n >> ");;
            var rawInput = System.Console.ReadLine();
            System.Console.WriteLine("You Wrote: " + rawInput);

            var inputList = new List<string>();

            foreach (char c in rawInput.ToCharArray())
            {
                inputList.Add(c.ToString());
            }

            //var tokenizedInput = global::Program.Lexer.lexInput(inputList.ToArray());
        }
    }
}
