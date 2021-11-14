using System;
using System.Collections;
using System.Collections.Generic;
using Interpreter;
using Microsoft.FSharp.Collections;
using NUnit.Framework;

namespace WpfApp1.SaveLoad_Tests
{
    public class SaverTests
    {
        private static IEnumerable GenerateSaveVariableValid()
        {
            yield return new TestCaseData(new Tuple<string, FSharpList<Util.terminal>>(
                "a", ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Plus,
                    Util.terminal.NewNumber(1.0),
                }))).Returns(new[]
            {
                "VARIABLE: [a,[1+1]]"
            });
            yield return new TestCaseData(new Tuple<string, FSharpList<Util.terminal>>(
                "a", ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Times,
                    Util.terminal.NewNumber(1.0),
                }))).Returns(new[]
            {
                "VARIABLE: [a,[1*1]]"
            });
            yield return new TestCaseData(new Tuple<string, FSharpList<Util.terminal>>(
                "word", ListModule.OfSeq(new List<Util.terminal>
                {
                    Util.terminal.NewNumber(1.0),
                    Util.terminal.Plus,
                    Util.terminal.NewNumber(1.0),
                }))).Returns(new[]
            {
                "VARIABLE: [word,[1+1]]"
            });
        }

        [TestCaseSource(nameof(GenerateSaveVariableValid))]
        [Test]
        public string[] GivenGenerateSaveVariables_WhenProvidedValidVariable_ReturnCorrectString(Tuple<string, FSharpList<Util.terminal>> input)
        {
            var (item1, item2) = input;
            var inputDict = new Dictionary<string, FSharpList<Util.terminal>> {{item1, item2}};
            return SaverLoader.Saver.GenerateSaveVariables(inputDict);
        }

        private static IEnumerable GenerateInvalidSaveData()
        {
            yield return new TestCaseData(new ValueTuple<string, IDictionary<string, FSharpList<Util.terminal>>>(
                ">>", new Dictionary<string, FSharpList<Util.terminal>>()));
            yield return new TestCaseData(new ValueTuple<string, IDictionary<string, FSharpList<Util.terminal>>>(
                null, new Dictionary<string, FSharpList<Util.terminal>>()));
        }
        
        [TestCaseSource(nameof(GenerateInvalidSaveData))]
        [Test]
        public void GivenConstructSaveContents_WhenProvidedInvalidOrNullValue_ThrowSaveException((string, IDictionary<string, FSharpList<Util.terminal>>) vals)
        {
            Assert.Throws<SaveException>(() => SaverLoader.Saver.ConstructSaveContents(vals.Item1, vals.Item2));
        }

        private static IEnumerable GenerateSaveDataForGenerateSavableData()
        {
            yield return new TestCaseData(">> 1+1\n2\n>>", new Dictionary<string, FSharpList<Util.terminal>>()
            {
                {
                    "a", ListModule.OfSeq(new List<Util.terminal>
                    {
                        Util.terminal.NewNumber(1.0),
                        Util.terminal.Plus,
                        Util.terminal.NewNumber(1.0)
                    })
                },
                {
                    "b", ListModule.OfSeq( new List<Util.terminal>
                    {
                        Util.terminal.NewNumber(2.0),
                        Util.terminal.Times,
                        Util.terminal.NewNumber(2.0)
                    })
                }
            }).Returns(new[]
            {
                "VARIABLE: [a,[1+1]]",
                "VARIABLE: [b,[2*2]]",
                ">> 1+1\n2"
            });
            
            yield return new TestCaseData(">> ", new Dictionary<string, FSharpList<Util.terminal>>()
            {
                {
                    "a", ListModule.OfSeq(new List<Util.terminal>
                    {
                        Util.terminal.NewNumber(1.0),
                        Util.terminal.Plus,
                        Util.terminal.NewNumber(1.0)
                    })
                },
                {
                    "b", ListModule.OfSeq( new List<Util.terminal>
                    {
                        Util.terminal.NewNumber(2.0),
                        Util.terminal.Times,
                        Util.terminal.NewNumber(2.0)
                    })
                }
            }).Returns(new[]
            {
                "VARIABLE: [a,[1+1]]",
                "VARIABLE: [b,[2*2]]",
                ""
            });
            
            yield return new TestCaseData(">> 1+1\n2\n>>", new Dictionary<string, FSharpList<Util.terminal>>
            {}).Returns(new[]
            {
                ">> 1+1\n2"
            });
            
            
        }

        [TestCaseSource(nameof(GenerateSaveDataForGenerateSavableData))]
        [Test]
        public string[] GivenGenerateSaveData_WhenProvidedValidSavableInput_ReturnCorrectArray
            (string consoleContent, Dictionary<string, FSharpList<Util.terminal>> variableContent)
        {
            return SaverLoader.Saver.GenerateSavableInfo(consoleContent, variableContent);
            
        }
        




        [Test]
        public void GivenSaveContents_WhenProvidedNullNull_ThrowSaveException()
        {
            Assert.Throws<SaveException>(() => SaverLoader.Saver.ConstructSaveContents(null, null));
        }
    }
}