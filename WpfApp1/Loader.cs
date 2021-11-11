using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace WpfApp1
{
    public class Loader
    {
        private readonly Dictionary<string, FSharpList<Util.terminal>> _variables = new();
        private string ConsoleContent { get; set; }

        public static string DecideFileToLoad(string fileToLoad)
        {
            return fileToLoad;
        }
        
        public static string DecideFileToLoad()
        {
            var fileDialog = new OpenFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                Filter = "MyMathsPal Files (*.mmp)|*.mmp;",
                FilterIndex = 1,
                Multiselect = false
            };

            return fileDialog.ShowDialog() != true ? null : fileDialog.FileName;
        }


        public (bool, string, IDictionary<string, FSharpList<Util.terminal>>) Load(string loadFile)
        {
            
            
            //If no file is selected return, else load that file
            if (loadFile == null)
            {
                return (false, null, null);
            }

            try
            {
                foreach (var loadedLine in File.ReadLines(loadFile))
                {
                    //Make sure line is an assigned variable
                    if (loadedLine.StartsWith("VARIABLE"))
                    {
                        var (variableName, lexedOutput) = ExtractVariable(loadedLine);

                        _variables.Add(variableName, lexedOutput);
                    }
                    //If Line wasnt a variable, append to ConsoleContent
                    else
                    {
                        ConsoleContent += loadedLine + "\n";
                    }
                }
            }
            catch (Util.TokenizeError)
            {
                throw new LoadException("Error In Variable Section of file " + loadFile);
            }
            catch (Util.ScanError)
            {
                throw new LoadException("Error In Variable Section of file " + loadFile);
            }
        
            ConsoleContent += ">>";
            return (true, ConsoleContent, _variables);
        }

        public (string variableName, FSharpList<Util.terminal> lexedOutput) ExtractVariable(string inputLine)
        {
            
            var line = inputLine[10..];
            line = line[..^1];
            var dictArr = line.Split(",");
            var variableName = dictArr[0][1..];
            var lexableInput = dictArr[1];
            lexableInput = lexableInput.Replace("[", "").Replace("]", "");
            var inpList = lexableInput.Select(character => character.ToString()).ToList();

            var inpFList = ListModule.OfSeq(inpList);

            var lexedOutput = Lexer.lexer(inpFList);
            
            return (variableName, lexedOutput);
        }
    }

    public class LoadException : Exception
    {
        public LoadException(string message): base(message)
        {
        }
    }
}