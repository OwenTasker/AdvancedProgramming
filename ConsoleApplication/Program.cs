
namespace ConsoleApplication
{
    class Program
    {
        static void Main(string[] args)
        {
            System.Console.Write("Enter an arithmetic operation: \n >> ");;
            var arithmeticOperationInput = System.Console.ReadLine();
            System.Console.WriteLine("You Wrote: " + arithmeticOperationInput);


            var charArrayRepresentationOfArithmeticInput = arithmeticOperationInput.ToCharArray();

            var stringArrayRepresentationOfArithmeticInput = new string[arithmeticOperationInput.Length];

            for(int i = 0; i < stringArrayRepresentationOfArithmeticInput.Length; i++)
            {
                stringArrayRepresentationOfArithmeticInput[i] = charArrayRepresentationOfArithmeticInput[i].ToString();
            }

            //var tokenizedInput = global::Program.Lexer.lexInput(stringArrayRepresentationOfArithmeticInput);
        }
    }
}
