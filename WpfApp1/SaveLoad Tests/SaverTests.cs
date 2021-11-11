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
                "a", ListModule.OfSeq(new List<Util.terminal>()
                {
                   Util.terminal.NewNumber(1.0), 
                   Util.terminal.Plus, 
                   Util.terminal.NewNumber(1.0), 
                }))).Returns(new[]
            {
                "VARIABLE: [a,[1+1]]"
            });
            yield return new TestCaseData(new Tuple<string, FSharpList<Util.terminal>>(
                "a", ListModule.OfSeq(new List<Util.terminal>()
                {
                    Util.terminal.NewNumber(1.0), 
                    Util.terminal.Times, 
                    Util.terminal.NewNumber(1.0), 
                }))).Returns(new[]
            {
                "VARIABLE: [a,[1*1]]"
            });
            yield return new TestCaseData(new Tuple<string, FSharpList<Util.terminal>>(
                "word", ListModule.OfSeq(new List<Util.terminal>()
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
            return SaverLoader.GenerateSaveVariables(inputDict);
        }

        [Test]
        public void GivenSaveContents_WhenProvidedNullNull_ThrowSaveException()
        {
            Assert.Throws<SaveException>(() => SaverLoader.ConstructSaveContents(null, null));
        }
    }
}