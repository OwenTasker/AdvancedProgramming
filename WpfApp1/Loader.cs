using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace WpfApp1
{
    public class Loader : MainWindow
    {
        private readonly Dictionary<string, FSharpList<Util.terminal>> _variables = new();
        private string ConsoleContent { get; set; }


        public (bool, string, IDictionary<string, FSharpList<Util.terminal>>) Load()
        {
            var fileDialog = new OpenFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                Filter = "MyMathsPal Files (*.mmp)|*.mmp;",
                FilterIndex = 1,
                Multiselect = false
            };

            //If no file is selected return, else load that file
            if (fileDialog.ShowDialog() != true)
            {
                return (false, null, null);
            }

            try
            {
                //Get the path of specified file
                var filePath = fileDialog.FileName;
                foreach (var loadedLine in File.ReadLines(filePath))
                {
                    //Make sure line is an assigned variable
                    if (loadedLine.StartsWith("VARIABLE"))
                    {
                        var line = loadedLine[10..];
                        line = line[..^1];
                        var dictArr = line.Split(",");
                        var variableName = dictArr[0][1..];
                        var lexableInput = dictArr[1];
                        lexableInput = lexableInput.Replace("[", "").Replace("]", "");
                        var inpList = lexableInput.Select(character => character.ToString()).ToList();

                        var inpFList = ListModule.OfSeq(inpList);

                        var lexedOutput = Lexer.lexer(inpFList);

                        _variables.Add(variableName, lexedOutput);
                    }
                    //If Line wasnt a variable, print it to the console
                    else
                    {
                        ConsoleContent += loadedLine + "\n";
                    }
                }
            }
            catch (Util.TokenizeError)
            {
                throw new LoadException("Error In Variable Section of file " + fileDialog.FileName);
            }
            catch (Util.ScanError)
            {
                throw new LoadException("Error In Variable Section of file " + fileDialog.FileName);
            }

            ConsoleContent += ">>";
            return (true, ConsoleContent, _variables);
        }
    }

    public class LoadException : Exception
    {
        public LoadException(string message): base(message)
        {
        }
    }
}