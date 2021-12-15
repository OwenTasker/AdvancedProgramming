using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace WpfApp1
{
    /// <summary>
    /// Class to control saving and loading in MyMathsPal.
    /// </summary>
    public class SaverLoader : ISaverLoader
    {
        /// <summary>
        /// The interpreter to be used to control saving and loading.
        /// </summary>
        private readonly IInterpreter _interpreter;

        /// <summary>
        /// Constructor, initialises the interpreter.
        /// </summary>
        /// <param name="interpreter">The interpreter for this saverloader.</param>
        public SaverLoader(IInterpreter interpreter)
        {
            _interpreter = interpreter;
        }

        /// <summary>
        /// Method to control the loading of a file.
        /// </summary>
        /// <returns>
        ///     A tuple containing a boolean success value, a string representing terminal input of the file, and a
        ///     dictionary containing the execution environment.
        /// </returns>
        public (bool, string, IDictionary<string, FSharpList<Util.terminal>>) Load()
        {
            var loadFile = DecideFileToLoad();

            //If no file is selected return, else load that file
            if (loadFile == null)
            {
                return (false, null, null);
            }

            var (variables, consoleContents) = ReadFile(loadFile);

            return (true, consoleContents, variables);
        }

        /// <summary>
        /// Method to control the reading of a file, given a path representing a file to be loaded..
        /// </summary>
        /// <param name="loadFile">A string path to the file.</param>
        /// <returns>A list of terminal content in the file, and the execution environment.</returns>
        /// <exception cref="SaveLoadException">If an error is encountered in the reading of the file.</exception>
        private (Dictionary<string, FSharpList<Util.terminal>>, string) ReadFile(string loadFile)
        {
            var variables = new Dictionary<string, FSharpList<Util.terminal>>();
            var consoleContents = "";

            try
            {
                if (!loadFile[^4..].Equals(".mmp"))
                {
                    throw new SaveLoadException("Invalid File Opened: " + loadFile);
                }

                foreach (var loadedLine in File.ReadLines(loadFile))
                {
                    if (loadedLine.StartsWith("VARIABLE")) // If line marked as variable, add to variables
                    {
                        var (variableName, lexedOutput) = ExtractVariable(loadedLine);
                        variables.Add(variableName, lexedOutput);
                    }
                    else // Else append to console content
                    {
                        consoleContents += loadedLine + "\n";
                    }
                }
            }
            catch (Util.ParseError)
            {
                throw new SaveLoadException("Error In Variable Section of file: " + loadFile);
            }
            catch (FileNotFoundException)
            {
                throw new SaveLoadException("Could Not Find File Specified: Please Try Again");
            }

            consoleContents += ">>";
            return (variables, consoleContents);
        }

        /// <summary>
        /// Method to control the process of choosing a file to load in the user's file explorer.
        /// </summary>
        /// <returns>A string representing the path to the file.</returns>
        private static string DecideFileToLoad()
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

        /// <summary>
        /// Method to extract variables from a given file line.
        /// </summary>
        /// <param name="inputLine">The line from which a variable is to be extracted.</param>
        /// <returns>A tuple containing the variable name and its assignment.</returns>
        private (string variableName, FSharpList<Util.terminal> lexedOutput) ExtractVariable(string inputLine)
        {
            var line = inputLine[10..^1];
            var dictArr = line.Split(",");
            var variableName = dictArr[0][1..];
            var lexInput = dictArr[1][1..^1];

            return (variableName, _interpreter.GetTerminalListFromString(lexInput));
        }

        /// <summary>
        /// Method to control the action of saving contents of the terminal and execution environment.
        /// </summary>
        /// <param name="consoleContents">The console contents to be saved.</param>
        /// <param name="variableContents">The execution environment contents to be saved.</param>
        /// <exception cref="SaveLoadException">
        ///     If an exception is encountered in the format of the lines to save.
        /// </exception>
        public void Save(string consoleContents, IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            if (!(consoleContents != ">>" || variableContents.Count > 0) || consoleContents == null)
            {
                throw new SaveLoadException("Unable To Save: No Contents Or Variables");
            }

            var fileToSaveTo = DetermineFileToSaveTo("MyMathsPal File (*.mmp)|*.mmp", "");

            if (fileToSaveTo != null)
            {
                File.WriteAllLines(fileToSaveTo.FileName, GenerateSavableInfo(consoleContents, variableContents));
            }
        }

        /// <summary>
        /// Method to control the action of providing save file dialog.
        /// </summary>
        /// <param name="fileType">The file type to be used to generate the dialog.</param>
        /// <param name="defaultName">The name of the file.</param>
        /// <returns>A save file dialog object.</returns>
        public SaveFileDialog DetermineFileToSaveTo(string fileType, string defaultName)
        {
            var dialog = new SaveFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),

                FileName = defaultName,

                Filter = fileType
            };

            return dialog.ShowDialog() != true ? null : dialog;
        }

        /// <summary>
        /// Method to control the generation of saveable information based upon raw terminal and environment content.
        /// </summary>
        /// <param name="consoleContents">The terminal contents to be saved.</param>
        /// <param name="variableContents">The execution environment contents to be saved.</param>
        /// <returns>A string array representing content in saveable form.</returns>
        private string[] GenerateSavableInfo(string consoleContents,
            IDictionary<string, FSharpList<Util.terminal>> variableContents)
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

        /// <summary>
        /// Method to convert variable content in a saveable form.
        /// </summary>
        /// <param name="variableContents">The execution environment content.</param>
        /// <returns>A string array containing variable content in saveable form.</returns>
        private string[] GenerateSaveVariables(IDictionary<string, FSharpList<Util.terminal>> variableContents)
        {
            var idx = 0;
            var savedVariables = new string[variableContents.Count];

            foreach (var (key, value) in variableContents)
            {
                var values = value.Aggregate("[", (current, terminalVal) =>
                    current + _interpreter.GetStringFromTerminal(terminalVal));
                values += "]";
                var text = $"VARIABLE: [{key},{values}]";
                savedVariables[idx++] = text;
            }

            return savedVariables;
        }

        /// <summary>
        /// Class representing an exceptional state arising in saving or loading.
        /// </summary>
        private class SaveLoadException : Exception
        {
            /// <summary>
            /// Constructor, initialises the message for this saveloadexception
            /// </summary>
            /// <param name="message">The message for this saveloadexception</param>
            public SaveLoadException(string message) : base(message)
            {
            }
        }
    }
}