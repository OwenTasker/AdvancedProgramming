using System;
using System.Collections.Generic;
using System.Linq;
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

            var expectedResults = new List<Tuple<string, List<Util.terminal>>>
            {
                Tuple.Create("a", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0)
                }),
                Tuple.Create("b", new List<Util.terminal>
                {
                    Util.terminal.NewWord("word")
                }),
                Tuple.Create("c", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0),
                    Util.terminal.Plus,
                    Util.terminal.NewNumber(3.0),
                })
            };

            var areAsExpected = new List<bool>();

            var idx = 0;
            foreach (var (key, val) in loadedValue.Item3)
            {
                var isKeyEqual = key.Equals(expectedResults[idx].Item1);
                var isValueEqual = val.ToList().SequenceEqual(expectedResults[idx].Item2);

                if (isKeyEqual && isValueEqual)
                {
                    areAsExpected.Add(true);
                }
                else
                {
                    areAsExpected.Add(false);
                }

                idx += 1;

            }

            var areAllCorrect = areAsExpected.All(c => c);
            
            
            Assert.True(areAllCorrect);
        }
    }
}