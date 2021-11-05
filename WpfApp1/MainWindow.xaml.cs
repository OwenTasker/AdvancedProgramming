using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Input;
using Interpreter;
using Microsoft.FSharp.Collections;
using Microsoft.Win32;
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
        private IDictionary<string, FSharpList<Util.terminal>> _environment =
            new Dictionary<string, FSharpList<Util.terminal>>();

        public class Variable
        {
            public string Name { [UsedImplicitly] get; init; }
            public string Value { [UsedImplicitly] get; init; }
        }

        public MainWindow()
        {
            InitializeComponent();
        }

        private void UpdateVariableWindow()
        {
            var keyValuePairs = _environment.ToList();
            var variablesList = keyValuePairs.Select(pair => new Variable
                {Name = pair.Key, Value = Util.terminalListToString("", pair.Value)});
            varDisplay.ItemsSource = variablesList;
        }

        private void EnterButtonPress(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here..." || string.IsNullOrEmpty(inputText.Text) ||
                string.IsNullOrWhiteSpace(inputText.Text)) return;

            if (inputText.Text.Length >= 4 && inputText.Text.ToUpper()[..4] == "PLOT")
            {
                try
                {
                    var args = inputText.Text[5..^1];
                    var argsArray = args.Split(",");
                    var trimmedArgsArray = argsArray.ToList().Select(x => x.Trim());

                    var trimmedArgsArray1 = trimmedArgsArray.ToList();

                    if (trimmedArgsArray1.Count != 3)
                    {
                        throw new Util.ExecError();
                    }


                    var array = trimmedArgsArray as string[] ?? trimmedArgsArray1.ToArray();
                    var function = array.First();
                    var str = Regex.Replace(function, "[^a-zA-Z0-9]", "|");
                    var variables = str.Split("|");

                    var openVars = new HashSet<string>();

                    var min = double.Parse(array.ToList()[1]);
                    var max = double.Parse(array.ToList()[2]);

                    var range = max - min;
                    var step = range / 750.0;

                    var xArray = new double[750];

                    for (var i = 0; i < 750; i++)
                    {
                        xArray[i] = (i + 1) * step;
                    }

                    foreach (var variable in variables)
                    {
                        var fSharpList = FSharpList<string>.Empty;
                        var enumerable = fSharpList.Append(variable);
                        var lexed = Lexer.lexer(ListModule.OfSeq(enumerable));

                        if (!Exec.closed(lexed, Util.toMap(_environment)))
                        {
                            openVars.Add(variable);
                        }
                    }


                    if (openVars.Count > 2)
                    {
                        throw new Util.ExecError();
                    }


                    var funcStrings = function.Select(s => s.ToString()).ToList();
                    var funcAsFSharpList = ListModule.OfSeq(funcStrings);
                    var lexerOutput = Lexer.lexer(funcAsFSharpList);
                    Parser.expression(lexerOutput);
                    var (_, tempDict) = Exec.exec(lexerOutput,
                        Util.toMap(new Dictionary<string, FSharpList<Util.terminal>>()));
                    _environment.ToList().ForEach(x => tempDict.Add(x.Key, x.Value));

                    var yArray = new double[750];

                    for (var i = 0; i < 750; i++)
                    {
                        var query = "y(x->" + xArray[i] + ")";

                        var queryList = query.Select(c => c.ToString()).ToList();
                        var inputFSharpList = ListModule.OfSeq(queryList);
                        var lexedQuery = Lexer.lexer(inputFSharpList);
                        var (executedQuery, _) = Exec.exec(lexedQuery, Util.toMap(tempDict));

                        yArray[i] = double.Parse(Util.terminalListToString("", executedQuery));
                    }


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
                    var inputfSharpList = ListModule.OfSeq(inputList);
                    var lexerOutput = Lexer.lexer(inputfSharpList);
                    Parser.expression(lexerOutput);
                    consoleText.AppendText(" " + input + "\n");
                    consoleText.ScrollToEnd();
                    inputText.Clear();
                    var execOutput = Exec.exec(lexerOutput, Util.toMap(_environment));
                    consoleText.AppendText(Util.terminalListToString("", execOutput.Item1) + "\n>>");
                    _environment = execOutput.Item2;
                    UpdateVariableWindow();
                    inputText.Text = "Enter query here...";
                }
                catch (Util.TokenizeError exception)
                {
                    consoleText.AppendText(input + "\n" + exception.Data0 + "\n>>");
                }
                catch (Util.ScanError exception)
                {
                    consoleText.AppendText(input + "\n" + exception.Data0 + "\n>>");
                }
                catch (Util.ExecError)
                {
                    consoleText.AppendText("Expression cannot be executed\n");
                }
            }
        }

        private void EnterKeyClick(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                EnterButtonPress(this, new RoutedEventArgs());
                inputText.Clear();
            }
        }

        private void InputTextBoxRemovePrompt(object sender, RoutedEventArgs e)
        {
            if (inputText.Text == "Enter query here...")
            {
                inputText.Text = "";
            }
        }

        private void InputTextBoxAddPrompt(object sender, RoutedEventArgs e)
        {
            if (String.IsNullOrEmpty(inputText.Text) || String.IsNullOrWhiteSpace(inputText.Text))
            {
                inputText.Text = "Enter query here...";
            }
        }

        private void SaveButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            var newText = consoleText.Text;
            newText = newText.Replace("\n", "");
            var isValidToSave = Regex.Match(newText, "(>>.*){2}");

            //If not enough characters present to save, return, else continue
            if (!isValidToSave.Success)
            {
                MessageBox.Show("Insufficient Console Text To Save. Please execute at least one line");
                return;
            }

            var savableInfo = new string[_environment.Count + 1];
            var idx = 0;

            //Collect each variable and add them to savableInfo
            foreach (var (key, value) in _environment)
            {
                var text = $"[{key}, {value}]";
                savableInfo[idx] = text;
                idx += 1;
            }

            //Add console text to savableInfo
            savableInfo[idx] = consoleText.Text[..^3];

            var dialog = new SaveFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                Filter = "MyMathsPal File (*.mmp)|*.mmp"
            };

            if (dialog.ShowDialog() != true)
            {
                MessageBox.Show("Cancelled Save Operation");
                return;
            }

            File.WriteAllLines(dialog.FileName, savableInfo);
        }

        private void LoadButton_OnClick(object sender, RoutedEventArgs routedEventArgs)
        {
            var fileDialog = new OpenFileDialog
            {
                InitialDirectory = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments),
                Filter = "MyMathsPal Files (*.mmp)|*.mmp;",
                FilterIndex = 1,
                Multiselect = false
            };

            //If no file is selected return, else load that file
            if (fileDialog.ShowDialog() != true)
            {
                MessageBox.Show("Load Cancelled/Not Successful");
                return;
            }

            consoleText.Clear();
            _environment.Clear();
            UpdateVariableWindow();

            //Get the path of specified file
            var filePath = fileDialog.FileName;

            foreach (var loadedLine in File.ReadLines(filePath))
            {
                //Make sure line is an assigned variable
                if (loadedLine.Contains(','))
                {
                    var line = loadedLine[1..];
                    line = line[..^1];
                    var dictArr = line.Split(",");
                    var inputList = dictArr[1].Select(c => c.ToString()).ToList();
                    var inputfSharpList = ListModule.OfSeq(inputList);
                    var lexerOutput = Lexer.lexer(inputfSharpList);
                    _environment.Add(dictArr[0], lexerOutput);
                }
                else
                {
                    consoleText.Text += loadedLine + "\n";
                }
            }

            UpdateVariableWindow();
            consoleText.Text += ">>";
        }
    }
}