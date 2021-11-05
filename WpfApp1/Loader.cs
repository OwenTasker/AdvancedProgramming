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
                    if (!dictArr[1].StartsWith(" [Number"))
                    {
                        var inputList = dictArr[1].Select(c => c.ToString()).ToList();
                        var inputFSharpList = ListModule.OfSeq(inputList);
                        var lexerOutput = Lexer.lexer(inputFSharpList);
                        _variables.Add(dictArr[0], lexerOutput);
                    }
                    else
                    {
                        var dictArrCheckProcess = dictArr[1].Replace("]", "").Replace("[", "");
                        var terminalDict = dictArrCheckProcess.Split(" ");
                        if (terminalDict[1].Equals("Number"))
                        {
                            var inputList = terminalDict[2].Select(c => c.ToString()).ToList();
                            var inputFSharpList = ListModule.OfSeq(inputList);
                            var terminalVal = Lexer.lexer(inputFSharpList);
                            _variables.Add(dictArr[0].Replace("[", ""), terminalVal);
                        }
                        else
                        {
                            throw new LoadException("Unexpected item in loading area");
                        }
                    }
            
                }
                else
                {
                    ConsoleContent += loadedLine + "\n";
                }
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