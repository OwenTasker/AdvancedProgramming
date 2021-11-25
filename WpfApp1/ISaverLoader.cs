using System.Collections.Generic;
using Interpreter;
using Microsoft.FSharp.Collections;

namespace WpfApp1
{
    public interface ISaverLoader
    {
        public (bool, string, IDictionary<string, FSharpList<Util.terminal>>) Load();

        public void Save(string consoleContents,
            IDictionary<string, FSharpList<Util.terminal>> variableContents);
    }
}