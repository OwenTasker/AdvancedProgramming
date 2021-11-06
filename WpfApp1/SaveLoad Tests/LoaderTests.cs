using NUnit.Framework;

namespace WpfApp1.SaveLoad_Tests
{
    public class LoaderTests
    {
        static string[] ValidLoadCases =
        {
            "./SaveLoad Tests/TestingFiles/testJustValidVariables.mmp", 
            "./SaveLoad Tests/TestingFiles/testJustValidUserDefinedFunctions.mmp",
            "./SaveLoad Tests/TestingFiles/testValidVariablesAndUserDefinedFunctions.mmp"
        };

        
        [TestCaseSource(nameof(ValidLoadCases))]
        public void GivenLoad_WhenPassedValidSaveFile_ReturnCorrectOutcome(string filePath)
        {
            var response = Loader.DecideFileToLoad(filePath);
        }
    }
}