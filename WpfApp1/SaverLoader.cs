using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace WpfApp1
{
    public static class SaverLoader
    {
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
        
        public static (bool, string, IDictionary<string, FSharpList<Util.terminal>>) Load(string loadFile)
        {

            var variables = new Dictionary<string, FSharpList<Util.terminal>>();
            var consoleContents = "";
            
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

                        variables.Add(variableName, lexedOutput);
                    }
                    //If Line wasnt a variable, append to ConsoleContent
                    else
                    {
                        consoleContents += loadedLine + "\n";
                    }
                }
            }
            catch (Exception)
            {
                throw new LoadException("Error In Variable Section of file: " + loadFile);
            }
        
            consoleContents += ">>";
            return (true, consoleContents, variables);
        }

        public static (string variableName, FSharpList<Util.terminal> lexedOutput) ExtractVariable(string inputLine)
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

            if (Parser.parse(lexedOutput))
            {
                return (variableName, lexedOutput);
            }

            throw new Util.ParseError();
        }
        
        public static void SaveContents(string consoleContents, IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            var isValidToSaveConsoleContents = consoleContents != ">>";
            var isValidToSaveVariableContents = variableContents.Count > 0;

            if (!(isValidToSaveConsoleContents || isValidToSaveVariableContents))
            {
                throw new SaveException("Unable To Save: No Contents Or Variables");
            }
            
            var dialog = new SaveFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                
                Filter = "MyMathsPal File (*.mmp)|*.mmp"
            };

            if (dialog.ShowDialog() != true)
            {
                return;
            }

            var savableInfo = GenerateSavableInfo(consoleContents, variableContents);

            File.WriteAllLines(dialog.FileName, savableInfo);
        }

        private static string[] GenerateSavableInfo(string consoleContents, IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            var savableInfo = new string[variableContents.Count + 1];
            
            var variableInfo = GenerateSaveVariables(variableContents);

            for (var i = 0; i < variableInfo.Length; i++)
            {
                savableInfo[i] = variableInfo[i];
            }

            if (consoleContents != ">>")
            {
                savableInfo[^1] = consoleContents[..^3];
            }

            return savableInfo;
        }

        public static string[] GenerateSaveVariables(IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            var idx = 0;
            var savedVariables = new string[variableContents.Count];

            foreach (var (key, value) in variableContents)
            {
                var values = value.Aggregate("[", (current, terminalVal) =>
                    current + Util.individualTerminalToString(terminalVal));
                values += "]";
                var text = $"VARIABLE: [{key},{values}]";
                savedVariables[idx++] = text;
            }

            return savedVariables;
        }
    }

    public class LoadException : Exception
    {
        public LoadException(string message): base(message)
        {
        }
    }
    public class SaveException : Exception
    {
        public SaveException(string message): base(message)
        {
        }
    }
}
