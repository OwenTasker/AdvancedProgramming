using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Input;
using Interpreter;
using Microsoft.FSharp.Collections;
using JetBrains.Annotations;

namespace WpfApp1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// References:
    /// https://stackoverflow.com/questions/11873378/adding-placeholder-text-to-textbox
    /// https://stackoverflow.com/questions/34298857/check-whether-a-textbox-is-empty-or-not
    /// https://stackoverflow.com/questions/19975617/press-enter-in-textbox-to-and-execute-button-command
    /// https://stackoverflow.com/questions/14598024/make-textbox-uneditable
    /// https://stackoverflow.com/questions/18260702/textbox-appendtext-not-autoscrolling
    /// </summary>
    public partial class MainWindow
    {
        /// <summary>
        /// Execution environment for this session.
        /// </summary>
        private IDictionary<string, FSharpList<Util.terminal>> _environment =
            new Dictionary<string, FSharpList<Util.terminal>>();

        /// <summary>
        /// Class to represent a user defined variable.
        /// </summary>
        private class Variable
        {
            /// <summary>
            /// Identifier of the variable.
            /// </summary>
            public string Name { [UsedImplicitly] get; init; }
            /// <summary>
            /// Value held by the variable.
            /// </summary>
            public string Value { [UsedImplicitly] get; init; }
        }

        /// <summary>
        /// Entry point, initializes the app window.
        /// </summary>
        public MainWindow()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Method to refresh the variable pane, to be called whenever an action might cause the environment to update.
        /// </summary>
        private void UpdateVariableWindow()
        {
            var keyValuePairs = _environment.ToList();
            var variablesList = keyValuePairs.Select(pair => new Variable
                {Name = pair.Key, Value = Util.terminalListToString("", pair.Value)});
            varDisplay.ItemsSource = variablesList;
        }

        /// <summary>
        /// Method to control user submission of a statement to the application.
        /// </summary>
        private void EnterButtonPress(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here..." || string.IsNullOrEmpty(inputText.Text) ||
                string.IsNullOrWhiteSpace(inputText.Text)) return;
            
            if (inputText.Text.Length >= 4 && inputText.Text.ToUpper()[..4] == "PLOT")
            {
                consoleText.AppendText(inputText.Text+"\n" + ">>");
                try
                {
                    var trimmedArgsArray = TrimmedArgsArray(inputText.Text);

                    var xArray = ComputeXArray(trimmedArgsArray);
                    var yArray = ComputeYArray(trimmedArgsArray, xArray);
                    
                    var graphPopUp = new GraphPopUp(xArray, yArray);
                    graphPopUp.Show();
                }
                catch (Exception plottingException)
                {
                    consoleText.AppendText("Plotting Exception: " + plottingException.Message + "\n" +
                                           plottingException.StackTrace + "\n>>");
                }
            }
            else
            {
                // 1. send query to lexer/parser/executor
                // 2a. if valid, put received answer on new line
                // 2b. if invalid, put out error
                var input = inputText.Text;

                try
                {
                    var inputList = input.Select(c => c.ToString()).ToList();
                    var inputFSharpList = ListModule.OfSeq(inputList);
                    var lexerOutput = Lexer.lexer(inputFSharpList);
                    Parser.expression(lexerOutput);
                    consoleText.AppendText(" " + input + "\n");
                    consoleText.ScrollToEnd();
                    inputText.Clear();
                    var (item1, item2) = Exec.exec(lexerOutput, Util.toMap(_environment));
                    consoleText.AppendText(Util.terminalListToString("", item1) + "\n>>");
                    _environment = item2;
                    UpdateVariableWindow();
                    inputText.Text = "Enter query here...";
                }
                catch (Util.TokenizeError ex1)
                {
                    consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
                }
                catch (Util.ScanError ex1)
                {
                    consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
                }
                catch (Util.ParseError ex1)
                {
                    consoleText.AppendText(input + "\n\"" + input + "\"\n" + ex1.Data0 + "\n>>");
                }
                catch (Exception ex)
                {
                    consoleText.AppendText("Expression cannot be executed\n" + ex.Message);
                }
            }
        }
        
        private IDictionary<string, FSharpList<Util.terminal>> CreateExecutionEnvironment(string function)
        {
            var funcStrings = function.Select(s => s.ToString()).ToList();
            
            var funcAsFSharpList = ListModule.OfSeq(funcStrings);
            var lexerOutput = Lexer.lexer(funcAsFSharpList);
            
            Parser.expression(lexerOutput);

            
            
            var (_, item2) = Exec.exec(lexerOutput,
                Util.toMap(new Dictionary<string, FSharpList<Util.terminal>>()));

            var tempDict = item2.ToList().ToDictionary(
                pair => pair.Key, pair => pair.Value);

            _environment.ToList().ForEach(x => tempDict.Add(x.Key, x.Value));
            
            return tempDict;
        }

        private double[] ComputeYArray(IReadOnlyList<string> trimmedArgsArray, IReadOnlyList<double> xArray)
        {
            var yArray = new double[750];

            // Get variables from function
            var openVars = GetOpenVariables(trimmedArgsArray[0]);

            var environment = CreateExecutionEnvironment(trimmedArgsArray[0]);

            for (var i = 0; i < 750; i++)
            {
                string query;
                if (openVars.Count == 1)
                {
                    query = openVars[0] + "()";
                }
                else
                {
                    query = openVars[0] + "(" + openVars[1] + "->" + xArray[i] + ")";
                }
                
                var queryList = query.Select(c => c.ToString()).ToList();
                var inputFSharpList = ListModule.OfSeq(queryList);
                var lexedQuery = Lexer.lexer(inputFSharpList);

                var (executedQuery, _) = Exec.exec(lexedQuery, Util.toMap(environment));

                yArray[i] = double.Parse(Util.terminalListToString("", executedQuery));
            }

            return yArray;
        }

        private static double[] ComputeXArray(IReadOnlyList<string> trimmedArgsArray)
        {
            var min = double.Parse(trimmedArgsArray[1]);
            var max = double.Parse(trimmedArgsArray[2]);
                
            if (min > max)
            {
                throw new Util.ExecError();
            }
            var range = max - min;
            var step = range / 750.0;
            
            var xArray = new double[750];
            for (var i = 0; i < 750; i++)
            {
                xArray[i] = (i + 1) * step;
            }

            return xArray;
        }

        private List<string> GetOpenVariables(string function)
        {
            var variables = Regex.Replace(
                function, "[^a-zA-Z]", "|").Split("|").Where(
                s => s.Length > 0).ToArray();

            // Compile set of open variables
            var openVars = new List<string>();

            var count = 0;

            foreach (var t in variables)
            {
                var fSharpList = FSharpList<string>.Empty;
                var enumerable = fSharpList.Append(t);
                var lexed = Lexer.lexer(ListModule.OfSeq(enumerable));

                if (count > 0)
                    if (Exec.closed(lexed, Util.toMap(_environment)) || openVars.Contains(t)) 
                        continue;

                openVars.Add(t);
                count++;
            }

            if (openVars.Count > 2)
            {
                throw new Util.ExecError();
            }

            return openVars;
        }

        private static List<string> TrimmedArgsArray(string input)
        {
            // Get plot parameters as an array
            var args = input[5..^1];
            var argsArray = args.Split(",");
            var trimmedArgsArray = argsArray.ToList().Select(x => x.Trim()).ToList();

            // If 3 parameters not passed then throw error
            if (trimmedArgsArray.Count != 3)
            {
                // Think about maybe just presenting a message instead
                throw new Util.ExecError();
            }

            return trimmedArgsArray;
        }

        /// <summary>
        /// Method to control user submission of a statement to the application.
        /// </summary>
        private void EnterKeyClick(object sender, KeyEventArgs e)
        {
            if (e.Key != Key.Enter) 
                return;
            
            EnterButtonPress(this, new RoutedEventArgs());
            inputText.Clear();
        }

        /// <summary>
        /// Method to control removal of dummy text from input box.
        /// </summary>
        private void InputTextBoxRemovePrompt(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here...")
            {
                inputText.Text = "";
            }
        }

        /// <summary>
        /// Method to control addition of dummy text to input box.
        /// </summary>
        private void InputTextBoxAddPrompt(object sender, RoutedEventArgs e)
        {
            if (string.IsNullOrEmpty(inputText.Text) || string.IsNullOrWhiteSpace(inputText.Text))
            {
                inputText.Text = "Enter query here...";
            }
        }

        /// <summary>
        /// Method to control action of Save button.
        /// </summary>
        private void SaveButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            try
            {
                var saver = new Saver(consoleText.Text, _environment);
                saver.SaveContents();
            }
            catch (SaveException e)
            {
                MessageBox.Show(e.Message);
            }
            
        }

        /// <summary>
        /// Method to control action of Load button.
        /// </summary>
        private void LoadButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            try
            {
                var loader = new Loader();
                var loadedVals = loader.Load();
                if (!loadedVals.Item1) return;
                _environment = loadedVals.Item3;
                consoleText.Text = loadedVals.Item2;
                UpdateVariableWindow();
            }
            catch (LoadException e)
            {
                MessageBox.Show(e.Message);
            }
        }

        /// <summary>
        /// Method to control action of Clear button.
        /// </summary>
        private void ClearButton_OnClick(object sender, RoutedEventArgs e)
        {
            consoleText.Text = ">>";
        }
    }
}