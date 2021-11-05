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
        private string ConsoleContents { get; set; }
        private IDictionary<string, FSharpList<Util.terminal>> VariableContents { get; set; }

        public Saver(string consoleContents, IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            this.ConsoleContents = consoleContents;
            this.VariableContents = variableContents;
        }

        public void SaveContents()
        {
            var isValidToSaveConsoleContents = ConsoleContents != ">>";
            var isValidToSaveVariableContents = VariableContents.Count > 0;

            var isValidToSave = isValidToSaveConsoleContents || isValidToSaveVariableContents;

            if (!isValidToSave)
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

            var savableInfo = new string[VariableContents.Count + 1];
            var idx = 0;

            foreach (var (key, value) in VariableContents)
            {
                var values = value.Aggregate("[", (current, VARIABLE) => current + Util.individualTerminalToString(VARIABLE));
                values += "]";
                var text = $"VARIABLE: [{key}, {values}]";
                savableInfo[idx++] = text;
            }

            if (isValidToSaveConsoleContents)
            {
                savableInfo[idx] = ConsoleContents[..^3];
            }

            File.WriteAllLines(dialog.FileName, savableInfo);
        }
    }

    public class SaveException : Exception
    {
        public SaveException(string message): base(message)
        {
        }
    }
}