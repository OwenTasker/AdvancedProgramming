using System;
using System.Collections.Generic;
using System.Linq;
using Interpreter;
using Microsoft.FSharp.Collections;
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
                    Util.terminal.NewNumber(3.0),
                    Util.terminal.Plus,
                    Util.terminal.NewNumber(3.0)
                }),
                Tuple.Create("c", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA")
                }),
                Tuple.Create("d", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testB"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0)
                }),
                Tuple.Create("e", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testC"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0),
                    Util.terminal.Times,
                    Util.terminal.NewNumber(345.0)
                }),
                Tuple.Create("f", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0), Util.terminal.Times, Util.terminal.NewWord("testD"), Util.terminal.Minus, 
                    Util.terminal.NewNumber(45.0), Util.terminal.Times, Util.terminal.NewNumber(345.0), Util.terminal.Plus,
                    Util.terminal.NewWord("testE"), Util.terminal.Exponent, Util.terminal.NewNumber(5.0)
                }),
                Tuple.Create("g", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0), Util.terminal.Times, Util.terminal.NewWord("testF"), Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0), Util.terminal.Times, Util.terminal.NewNumber(345.0), Util.terminal.Plus,
                    Util.terminal.NewWord("testG"), Util.terminal.Exponent, Util.terminal.NewNumber(5.0), Util.terminal.Minus,
                    Util.terminal.Lpar, Util.terminal.NewWord("testH"), Util.terminal.Divide, Util.terminal.NewNumber(43.0),
                    Util.terminal.Rpar
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

        private static IEnumerable<object[]> _loadTesting()
        {
            yield return 
                new object[]
                {
                    new Tuple<string, (string, List<Util.terminal>)>("VARIABLE [a, [1]]", ("a", new List<Util.terminal>
                    {
                        Util.terminal.NewNumber(1.0)
                    }))
                };
            yield return
                new object[]
                {
                    "1+1",
                    new List<Util.terminal>
                        {Util.terminal.NewNumber(1.0), Util.terminal.Plus, Util.terminal.NewNumber(1.0)}
                };
        }

        [TestCaseSource(nameof(_loadTesting))]
        [Test]
        public void GivenExtractVariable_ReturnCorrectTerminalRepresentation(IEnumerable<object> inputAndOutput)
        {
            
            var enumeratedType = inputAndOutput.Cast <Tuple<string, (string,List<Util.terminal>)>>().GetEnumerator();
            var expectedName = enumeratedType.Current?.Item1;
            var valueToLoad = enumeratedType.Current?.Item2.Item1;
            var expectedOutput = ListModule.OfSeq(enumeratedType.Current?.Item2.Item2);
            enumeratedType.Dispose();

            var loader = new Loader();
            loader.ExtractVariable(valueToLoad);
            


        }
    }
}