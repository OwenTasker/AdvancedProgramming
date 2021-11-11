using System;
using System.Collections;
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
            var loadFileVal =
                SaverLoader.DecideFileToLoad("../WpfApp1/SaveLoad Tests/TestingFiles/testJustValidVariables.mmp");
            var loadedValue = SaverLoader.Load(loadFileVal);

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
                    Util.terminal.NewNumber(3.0), Util.terminal.Times, Util.terminal.NewWord("testD"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0), Util.terminal.Times, Util.terminal.NewNumber(345.0),
                    Util.terminal.Plus,
                    Util.terminal.NewWord("testE"), Util.terminal.Exponent, Util.terminal.NewNumber(5.0)
                }),
                Tuple.Create("g", new List<Util.terminal>
                {
                    Util.terminal.NewNumber(3.0), Util.terminal.Times, Util.terminal.NewWord("testF"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0), Util.terminal.Times, Util.terminal.NewNumber(345.0),
                    Util.terminal.Plus,
                    Util.terminal.NewWord("testG"), Util.terminal.Exponent, Util.terminal.NewNumber(5.0),
                    Util.terminal.Minus,
                    Util.terminal.Lpar, Util.terminal.NewWord("testH"), Util.terminal.Divide,
                    Util.terminal.NewNumber(43.0),
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

        private static IEnumerable ExtractVariableLoadTesting()
        {
            yield return new TestCaseData("VARIABLE: [a, [1]]").Returns(("a", ListModule.OfSeq(new List<Util.terminal>
            {
                Util.terminal.NewNumber(1.0)
            })));
            yield return new TestCaseData("VARIABLE: [a, [1+1]]").Returns(("a", ListModule.OfSeq(new List<Util.terminal>
            {
                Util.terminal.NewNumber(1.0),
                Util.terminal.Plus,
                Util.terminal.NewNumber(1.0)
            })));
            yield return new TestCaseData("VARIABLE: [a, [1*testA]]").Returns(("a",
                ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA")
                })));
            yield return new TestCaseData("VARIABLE: [a, [1*testA-45]]").Returns(("a",
                ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0)
                })));
            yield return new TestCaseData("VARIABLE: [a, [1*testA-45*345]]").Returns(("a",
                ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0),
                    Util.terminal.Times,
                    Util.terminal.NewNumber(345.0),
                })));
            yield return new TestCaseData("VARIABLE: [a, [1*testA-45*345+testB]]").Returns(("a",
                ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0),
                    Util.terminal.Times,
                    Util.terminal.NewNumber(345.0),
                    Util.terminal.Plus,
                    Util.terminal.NewWord("testB"),
                })));
            yield return new TestCaseData("VARIABLE: [a, [1*testA-45*345+testB^5]]").Returns(("a",
                ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0),
                    Util.terminal.Times,
                    Util.terminal.NewNumber(345.0),
                    Util.terminal.Plus,
                    Util.terminal.NewWord("testB"),
                    Util.terminal.Exponent,
                    Util.terminal.NewNumber(5.0),
                })));
            yield return new TestCaseData("VARIABLE: [a, [1*testA-45*345+testB^5-(testC/43)]]").Returns(("a",
                ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewWord("testA"),
                    Util.terminal.Minus,
                    Util.terminal.NewNumber(45.0),
                    Util.terminal.Times,
                    Util.terminal.NewNumber(345.0),
                    Util.terminal.Plus,
                    Util.terminal.NewWord("testB"),
                    Util.terminal.Exponent,
                    Util.terminal.NewNumber(5.0),
                    Util.terminal.Minus,
                    Util.terminal.Lpar,
                    Util.terminal.NewWord("testC"),
                    Util.terminal.Divide,
                    Util.terminal.NewNumber(43.0),
                    Util.terminal.Rpar,
                })));
            
        }

        [TestCaseSource(nameof(ExtractVariableLoadTesting))]
        [Test]
        public (string, FSharpList<Util.terminal>) GivenExtractVariable_ReturnCorrectTerminalRepresentation(string line)
        {
            return SaverLoader.ExtractVariable(line);
        }

        private static IEnumerable ExtractMalformedVariableLoadTestingFailConditions()
        {
            yield return new TestCaseData("VARIABLE: [a, [+]]");
            yield return new TestCaseData("VARIABLE: [a, [-]]");
            yield return new TestCaseData("VARIABLE: [a, [/]]");
            yield return new TestCaseData("VARIABLE: [a, []]");
            yield return new TestCaseData("VARIABLE: [a, [(1+3]]");
            yield return new TestCaseData("VARIABLE: [a, [1+3)]]");
            
        }
        
        [TestCaseSource(nameof(ExtractMalformedVariableLoadTestingFailConditions))]
        [Test]
        public void GivenMalformedExtractVariable_ThrowParseError(string line)
        {
            
            Assert.Throws<Util.ParseError>(() => SaverLoader.ExtractVariable(line));
        }
        
        private static IEnumerable ExtractInvalidTokenVariableLoadTestingFailConditions()
        {
            yield return new TestCaseData("VARIABLE: [a, [1+1?]]");
            yield return new TestCaseData("VARIABLE: [a, [1@1]]");
            yield return new TestCaseData("VARIABLE: [a, [1~1]]");
        }
        
        [TestCaseSource(nameof(ExtractInvalidTokenVariableLoadTestingFailConditions))]
        [Test]
        public void GivenInvalidTokenExtractVariable_ThrowParseError(string line)
        {
            Assert.Throws<Util.TokenizeError>(() => SaverLoader.ExtractVariable(line));
        }
        
        private static IEnumerable ExtractMalformedTokensVariableLoadTestingFailConditions()
        {
            yield return new TestCaseData("VARIABLE: [a, [.]]");
        }
        
        [TestCaseSource(nameof(ExtractMalformedTokensVariableLoadTestingFailConditions))]
        [Test]
        public void GivenMalformedTokenExtractVariable_ThrowScanError(string line)
        {
            Assert.Throws<Util.ScanError>(() => SaverLoader.ExtractVariable(line));
        }
    }
}