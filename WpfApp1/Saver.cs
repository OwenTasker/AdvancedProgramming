using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace WpfApp1
{
    public class Saver
    {
        
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

            var savableInfo = SavableInfo(isValidToSaveConsoleContents, consoleContents, variableContents);

            File.WriteAllLines(dialog.FileName, savableInfo);
        }

        private static string[] SavableInfo(bool isValidToSaveContents, string consoleContents, IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            var savableInfo = new string[variableContents.Count + 1];
            
            var idx = SaveVariables(savableInfo, variableContents);

            if (isValidToSaveContents)
            {
                savableInfo[idx] = consoleContents[..^3];
            }

            return savableInfo;
        }

        private static int SaveVariables(string[] savableInfo, IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            var idx = 0;

            foreach (var (key, value) in variableContents)
            {
                var values = value.Aggregate("[", (current, terminalVal) =>
                    current + Util.individualTerminalToString(terminalVal));
                values += "]";
                var text = $"VARIABLE: [{key},{values}]";
                savableInfo[idx++] = text;
            }

            return idx;
        }
    }

    public class SaveException : Exception
    {
        public SaveException(string message): base(message)
        {
        }
    }
}