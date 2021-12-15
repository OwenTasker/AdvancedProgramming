using System.Collections.Generic;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;

namespace WpfApp1
{
    /// <summary>
    /// Interface to represent the contract to be fulfilled for a saverloader in MyMathsPal.
    /// </summary>
    public interface ISaverLoader
    {
        /// <summary>
        /// Method to load a file in the MyMathsPal system.
        /// </summary>
        /// <returns>
        ///     A tuple containing a boolean success value, a string representing terminal input of the file, and a
        ///     dictionary containing the execution environment.
        /// </returns>
        public (bool, string, IDictionary<string, FSharpList<Util.terminal>>) Load();

        /// <summary>
        /// Method to save a file in the MyMathsPal system.
        /// </summary>
        /// <param name="consoleContents">A string representing the current console contents.</param>
        /// <param name="variableContents">A dictionary representing the current execution environment.</param>
        public void Save(string consoleContents,
            IDictionary<string, FSharpList<Util.terminal>> variableContents);

        /// <summary>
        /// Method to control provision of save file dialog.
        /// </summary>
        /// <param name="fileType">The file type to be saved.</param>
        /// <param name="defaultName">The default name to be used.</param>
        /// <returns>A save file dialog.</returns>
        public SaveFileDialog DetermineFileToSaveTo(string fileType, string defaultName);
    }
}