using System.Collections.Generic;
using Interpreter;
using NUnit.Framework;

namespace WpfApp1.SaveLoad_Tests
{
    public class LoaderTests
    {
  
        [Test]
        public void GivenLoad_WhenPassedValidSaveFileWithSingleVariable_ReturnCorrectOutcome()
        {
            var loader = new Loader();
            var loadFileVal = Loader.DecideFileToLoad("../WpfApp1/SaveLoad Tests/TestingFiles/testJustValidVariables.mmp");
            var loadedValue = loader.Load(loadFileVal);
            var keys = new List<string>();
            var vals = new List<Util.terminal>();
            foreach (var (key, val) in loadedValue.Item3)
            {
                keys.Add(key);
                vals.Add(val.Head);
                
            }
            
            var isFirstVariableCorrect = keys[0].Equals("a") &&  vals[0].Equals(Util.terminal.NewNumber(3.0));
            var isSecondVariableCorrect = keys[1].Equals("b") && vals[1].Equals(Util.terminal.NewWord("word"));
            
            Assert.True(isFirstVariableCorrect && isSecondVariableCorrect);
        }
    }
}